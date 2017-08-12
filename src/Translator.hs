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
    , issueToken
    , issueAuth
    , initTransData
    , initTransDataWith
    , checkTransData

    -- ** Translation
    -- *** ExceptT variants
    , translate
    , translateArray
    , translateArrayText
    , translateArraySentences

    -- *** IO variants
    , issueAuthIO
    , initTransDataIO
    , checkTransDataIO
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

import           Control.Monad.Except
import           Data.Text               as T (Text, splitAt)
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS


-- | Simplest possible translation function.
--   Always needs to make a request for the JWT token first.
simpleTranslate :: SubscriptionKey -> Manager
                -> Maybe Language -> Language
                -> Text -> IO (Either TranslatorException Text)
simpleTranslate key man from to txt = runExceptT $ do
        tok <- ExceptT $ issueToken man key
        ExceptT $ basicTranslate man tok from to txt


-- | An 'AuthToken' together with the time it was recieved.
--   Each token is valid for 10 minutes.
data AuthData = AuthData
    { timeStamp :: UTCTime
    , authToken :: AuthToken
    } deriving Show


-- | Retrieve a token, as in 'issueToken', and save it together with a timestamp.
issueAuth :: Manager -> SubscriptionKey -> ExceptT TranslatorException IO AuthData
issueAuth man key = do
    tok <- ExceptT $ issueToken man key
    now <- liftIO getCurrentTime
    pure $ AuthData now tok


-- | The data to hold onto for making translation requests.
--   Includes your 'SubscriptionKey', an `AuthData` and an HTTPS 'Manager'.
data TransData = TransData
    { subKey   :: SubscriptionKey
    , manager  :: Manager
    , authData :: AuthData }

-- | Retrieve an 'AuthData' token and hold on to the new HTTPS manager.
initTransData :: SubscriptionKey -> ExceptT TranslatorException IO TransData
initTransData key =
    liftIO (newManager tlsManagerSettings) >>= initTransDataWith key

-- | Retrieve an 'AuthData' token and hold on to the HTTPS manager.
--   For when you want to supply a particular manager. Otherwise use 'initTransData'.
initTransDataWith :: SubscriptionKey -> Manager -> ExceptT TranslatorException IO TransData
initTransDataWith key man =
    TransData key man <$> issueAuth man key

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkTransData :: TransData -> ExceptT TranslatorException IO TransData
checkTransData tdata = do
    now <- liftIO getCurrentTime
    let before = timeStamp $ authData tdata
    auth <- if diffUTCTime now before > 9*60+30
        then issueAuth (manager tdata) (subKey tdata)
        else pure (authData tdata)
    pure $ tdata { authData = auth }

-- | Translate text
translate :: TransData -> Maybe Language -> Language -> Text
          -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     td <- checkTransData tdata
     ExceptT $ basicTranslate (manager td) (authToken $ authData td) from to txt

-- | Translate a text array.
--   The 'ArrayResponse' you get back includes sentence break information.
translateArray :: TransData -> Language -> Language -> [Text]
               -> ExceptT TranslatorException IO ArrayResponse
translateArray tdata from to txts = do
     td <- checkTransData tdata
     ExceptT $ basicTranslateArray (manager td) (authToken $ authData td) from to txts

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
    } deriving (Show, Eq)

extractSentences :: [Int] -> Text -> [Text]
extractSentences []     txt = [txt]
extractSentences (n:ns) txt = headTxt : extractSentences ns tailTxt
    where (headTxt, tailTxt) = T.splitAt n txt

-- | Take the original texts and the ArrayResponse object, and apply the sentence break
--   information to pair each sentence in the request to the translated text.
mkSentences :: [Text] -> ArrayResponse -> [[Sentence]]
mkSentences origTxts (ArrayResponse tItems) =
    flip fmap (origTxts `zip` tItems) $
        \(origTxt, TransItem transTxt origBreaks transBreaks) ->
            zipWith Sentence
                (extractSentences origBreaks  origTxt)
                (extractSentences transBreaks transTxt)


-- | Retrieve a token, as in 'issueToken' and save it together with a timestamp.
issueAuthIO :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthData)
issueAuthIO man = runExceptT . issueAuth man

-- | Retrieve an 'AuthData' token and start up an HTTPS manager.
initTransDataIO :: SubscriptionKey -> IO (Either TranslatorException TransData)
initTransDataIO = runExceptT . initTransData

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkTransDataIO :: TransData -> IO (Either TranslatorException TransData)
checkTransDataIO = runExceptT . checkTransData

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
