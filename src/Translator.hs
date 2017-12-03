{-# LANGUAGE DeriveGeneric #-}

module Translator (

    -- * Basic Types
      SubscriptionKey (..)
    , AuthToken
    , AuthData (..)
    , TransData

    , Language (..)
    , TranslatorException

    , ArrayResponse (..)
    , TransItem (..)
    , Sentence (..)

    -- * API functions
    -- ** Authorization
    , lookupSubKey
    , issueToken
    , issueAuth
    , refresh
    , initTransData
    , initTransDataWith
    , checkAuth
    , keepFreshAuth

    -- ** Translation
    -- *** ExceptT variants
    , translate
    , translateArray
    , translateArrayText
    , translateArraySentences

    -- *** IO variants
    , lookupSubKeyIO
    , issueAuthIO
    , initTransDataIO
    , checkAuthIO
    , translateIO
    , translateArrayIO
    , translateArrayTextIO
    , translateArraySentencesIO

    -- *** Minimalistic variants
    , simpleTranslate
    , basicTranslate
    , basicTranslateArray

    -- * Pure functions
    , mkSentences

) where

import           Translator.API
import           Translator.API.Auth
import           Translator.Exception

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad.Except
import           Data.Char               (isSpace)
import           Data.IORef
import           Data.Monoid             ((<>))
import           Data.String             (fromString)
import           Data.Text               as T (Text, all, splitAt)
import           Data.Time
import           GHC.Generics            (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment      (lookupEnv)


-- | Simplest possible translation function.
--   Always needs to make a request for the JWT token first.
simpleTranslate :: SubscriptionKey -> Manager
                -> Maybe Language -> Language
                -> Text -> IO (Either TranslatorException Text)
simpleTranslate key man from to txt = runExceptT $ do
        tok <- ExceptT $ issueToken man key
        ExceptT $ basicTranslate man tok from to txt

-- | Retrieve your subscription key from the TRANSLATOR_SUBSCRIPTION_KEY environment
--   variable.
lookupSubKey :: ExceptT TranslatorException IO SubscriptionKey
lookupSubKey = ExceptT $
    maybe (Left MissingSubscriptionKey) (Right . SubKey . fromString) <$>
        lookupEnv "TRANSLATOR_SUBSCRIPTION_KEY"

-- | Retrieve your subscription key from the TRANSLATOR_SUBSCRIPTION_KEY environment
--   variable.
lookupSubKeyIO :: IO (Either TranslatorException SubscriptionKey)
lookupSubKeyIO = runExceptT lookupSubKey


-- | An 'AuthToken' together with the time it was recieved.
--   Each token is valid for 10 minutes.
data AuthData = AuthData
    { timeStamp :: UTCTime
    , authToken :: AuthToken
    } deriving Show


-- | Retrieve a token, via 'issueToken', and save it together with a timestamp.
issueAuth :: Manager -> SubscriptionKey -> ExceptT TranslatorException IO AuthData
issueAuth man key = do
    tok <- ExceptT $ issueToken man key
    now <- liftIO getCurrentTime
    pure $ AuthData now tok


-- | The data to hold onto for making translation requests.
--   Includes your 'SubscriptionKey', an `AuthData` and an HTTPS 'Manager'.
data TransData = TransData
    { subKey      :: SubscriptionKey
    , manager     :: Manager
    , authDataRef :: IORef AuthData }

-- | Retrieve an 'AuthData' token and hold on to the new HTTPS manager.
initTransData :: SubscriptionKey -> ExceptT TranslatorException IO TransData
initTransData key =
    liftIO (newManager tlsManagerSettings) >>= initTransDataWith key

-- | Retrieve an 'AuthData' token and hold on to the HTTPS manager.
--   For when you want to supply a particular manager. Otherwise use 'initTransData'.
initTransDataWith :: SubscriptionKey -> Manager -> ExceptT TranslatorException IO TransData
initTransDataWith key man =
    TransData key man <$> (issueAuth man key >>= liftIO . newIORef)


refresh :: TransData -> ExceptT TranslatorException IO AuthData
refresh tdata = do
    auth <- issueAuth (manager tdata) (subKey tdata)
    liftIO $ writeIORef (authDataRef tdata) auth
    pure auth

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkAuth :: TransData -> ExceptT TranslatorException IO AuthData
checkAuth tdata = do
    now <- liftIO getCurrentTime
    auth <- liftIO . readIORef $ authDataRef tdata
    if (diffUTCTime now (timeStamp auth) > 9*60+30)
        then refresh tdata
        else pure auth

-- | Create a 'TransData' with a new auth token and fork a thread to refresh it every
--   9 minutes.
--   This is mostly a quick-and-dirty function for demo purposes and one-off projects.
--   You'll want to roll something more robust for production applications.
keepFreshAuth :: SubscriptionKey -> ExceptT TranslatorException IO TransData
keepFreshAuth key = do
    tdata <- initTransData key
    _ <- liftIO . forkIO $ loop tdata
    pure tdata

    where
        loop :: TransData -> IO ()
        loop td = do
            threadDelay $ 10^(6::Int) * 9 * 60
            _ <- runExceptT $ refresh td
            loop td


-- | Translate text
translate :: TransData -> Maybe Language -> Language -> Text
          -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     tok <- authToken <$> checkAuth tdata
     ExceptT $ basicTranslate (manager tdata) tok from to txt

-- | Translate a text array.
--   The 'ArrayResponse' you get back includes sentence break information.
translateArray :: TransData -> Language -> Language -> [Text]
               -> ExceptT TranslatorException IO ArrayResponse
translateArray tdata from to txts = do
     tok <- authToken <$> checkAuth tdata
     ExceptT $ basicTranslateArray (manager tdata) tok from to txts

-- | Translate a text array, and just return the list of texts.
translateArrayText :: TransData -> Language -> Language -> [Text]
                   -> ExceptT TranslatorException IO [Text]
translateArrayText tdata from to txts =
    map transText . getArrayResponse <$> translateArray tdata from to txts

-- | Translate a text array, and split all the texts into constituent sentences,
--   paired with the originals.
translateArraySentences :: TransData -> Language -> Language -> [Text]
                        -> ExceptT TranslatorException IO [[Sentence]]
translateArraySentences tdata from to txts =
    mkSentences txts <$> translateArray tdata from to txts

-- | An original/translated sentence pair.
data Sentence = Sentence
    { fromText :: Text
    , toText   :: Text
    } deriving (Show, Eq, Generic)

extractSentences :: [Int] -> Text -> [Text]
extractSentences []     txt = [txt]
extractSentences (n:ns) txt = headTxt : extractSentences ns tailTxt
    where (headTxt, tailTxt) = T.splitAt n txt

-- | Take the original texts and the ArrayResponse object, and apply the sentence break
--   information to pair each sentence in the request to the translated text.
mkSentences :: [Text] -> ArrayResponse -> [[Sentence]]
mkSentences origTxts (ArrayResponse tItems) =
    uncurry formSentenceSet <$> zip origTxts tItems
    where
        formSentenceSet :: Text -> TransItem -> [Sentence]
        formSentenceSet origTxt (TransItem transTxt origBreaks transBreaks) =
            filter notBlank $ zipWith Sentence
                (extractSentences origBreaks  origTxt)
                (extractSentences transBreaks transTxt)

        notBlank :: Sentence -> Bool
        notBlank (Sentence orig trans) = not . T.all isSpace $ orig <> trans



-- | Retrieve a token, via 'issueToken', and save it together with a timestamp.
issueAuthIO :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthData)
issueAuthIO man = runExceptT . issueAuth man

-- | Retrieve an 'AuthData' token and start up an HTTPS manager.
initTransDataIO :: SubscriptionKey -> IO (Either TranslatorException TransData)
initTransDataIO = runExceptT . initTransData

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkAuthIO :: TransData -> IO (Either TranslatorException AuthData)
checkAuthIO = runExceptT . checkAuth

-- | Translate text.
translateIO :: TransData -> Maybe Language -> Language -> Text
            -> IO (Either TranslatorException Text)
translateIO tdata from to = runExceptT . translate tdata from to

-- | Translate a text array.
translateArrayIO :: TransData -> Language -> Language -> [Text]
                 -> IO (Either TranslatorException ArrayResponse)
translateArrayIO tdata from to = runExceptT . translateArray tdata from to

-- | Translate a text array, and just return the list of texts.
translateArrayTextIO :: TransData -> Language -> Language -> [Text]
                     -> IO (Either TranslatorException [Text])
translateArrayTextIO tdata from to =
    runExceptT . translateArrayText tdata from to

-- | Translate a text array, and split all the texts into constituent sentences
--   paired with the originals.
translateArraySentencesIO :: TransData -> Language -> Language -> [Text]
                          -> IO (Either TranslatorException [[Sentence]])
translateArraySentencesIO tdata from to =
    runExceptT . translateArraySentences tdata from to
