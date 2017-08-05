{-# LANGUAGE OverloadedStrings #-}

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
    , checkTransData

    -- ** Translation
    -- *** ExceptT variants
    , translate
    , translateArray
    , translateArraySentences

    -- *** IO variants
    , issueAuthIO
    , initTransDataIO
    , checkTransDataIO
    , translateIO
    , translateArrayIO
    , translateArraySentencesIO

    -- *** Minimalistic variants
    , simpleTranslate
    , basicTranslate
    , basicTranslateArray

    -- * Pure functions
    , mkSentences

    , tryIt

) where

import           Translator.API
import           Translator.API.Auth

import           Control.Exception
import           Control.Monad.Except
import           Data.Text as T (splitAt, Text)
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS


-- | Simplest possible translation function.
--   Always needs to make a request for the JWT token first.
simpleTranslate :: SubscriptionKey -> Manager
                -> Maybe Language -> Language
                -> Text -> IO (Either TranslatorException Text)
simpleTranslate key man from to txt =
    try $ do
        tok <- issueToken man key          >>= either throw pure
        basicTranslate man tok from to txt >>= either throw pure


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

-- | Retrieve an 'AuthData' token and hold on to the HTTPS manager.
initTransData :: SubscriptionKey -> ExceptT TranslatorException IO TransData
initTransData key = do
    man <- liftIO $ newManager tlsManagerSettings
    auth <- issueAuth man key
    pure $ TransData key man auth

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkTransData :: TransData -> ExceptT TranslatorException IO TransData
checkTransData tdata = do
    now <- liftIO getCurrentTime
    let before = timeStamp $ authData tdata
    auth <- if diffUTCTime now before > 9*60
        then issueAuth (manager tdata) (subKey tdata)
        else pure (authData tdata)
    pure $ tdata { authData = auth }

-- | Translate text
translate :: TransData -> Maybe Language -> Language -> Text
          -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     td <- checkTransData tdata
     ExceptT $ basicTranslate (manager td) (authToken $ authData td) from to txt

-- | Translate text array.
--   The 'ArrayResponse' you get back includes sentence break information.
translateArray :: TransData -> Language -> Language -> [Text]
               -> ExceptT TranslatorException IO ArrayResponse
translateArray tdata from to txts = do
     td <- checkTransData tdata
     ExceptT $ basicTranslateArray (manager td) (authToken $ authData td) from to txts

extractSentences :: [Int] -> Text -> [Text]
extractSentences []     txt = [txt]
extractSentences (n:ns) txt = headTxt : extractSentences ns tailTxt
    where (headTxt, tailTxt) = T.splitAt n txt

mkSentences :: [Text] -> ArrayResponse -> [[Sentence]]
mkSentences origTxts (ArrayResponse tItems) =
    flip fmap (origTxts `zip` tItems) $
        \(origTxt, TransItem transTxt origBreaks transBreaks) ->
            zipWith Sentence
                (extractSentences origBreaks  origTxt)
                (extractSentences transBreaks transTxt)


data Sentence = Sentence
    { fromText :: Text
    , toText   :: Text
    } deriving (Show, Eq)

-- | Translate text array, and split all texts into constituent sentences.
translateArraySentences :: TransData -> Language -> Language -> [Text]
                        -> ExceptT TranslatorException IO [[Sentence]]
translateArraySentences tdata from to txts =
    mkSentences txts <$> translateArray tdata from to txts


-- | Retrieve a token, as in 'issueToken' and save it together with a timestamp.
issueAuthIO :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthData)
issueAuthIO man = runExceptT . issueAuth man

-- | Retrieve an 'AuthData' token and start up an HTTPS manager.
initTransDataIO :: SubscriptionKey -> IO (Either TranslatorException TransData)
initTransDataIO = runExceptT . initTransData

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkTransDataIO :: TransData -> IO (Either TranslatorException TransData)
checkTransDataIO = runExceptT . checkTransData

-- | Translate text
translateIO :: TransData -> Maybe Language -> Language -> Text
            -> IO (Either TranslatorException Text)
translateIO tdata from to = runExceptT . translate tdata from to

-- | Translate text array
translateArrayIO :: TransData -> Language -> Language -> [Text]
                 -> IO (Either TranslatorException ArrayResponse)
translateArrayIO tdata from to = runExceptT . translateArray tdata from to

-- | Translate text array, and split all texts into constituent sentences.
translateArraySentencesIO :: TransData -> Language -> Language -> [Text]
                          -> IO (Either TranslatorException [[Sentence]])
translateArraySentencesIO tdata from to =
    runExceptT . translateArraySentences tdata from to


---- temp testing stuff ----

tryIt :: IO (Either TranslatorException ArrayResponse) -- ArrayResponse
tryIt = do
    td <- either (error . show) pure =<<
        initTransDataIO "195856783a844974b3de54c22f245b43"
    translateArrayIO td Swedish English [testTxt, testTxt2]
    -- putStrLn "\n"
    -- Right ar <- translateArrayIO man token Swedish English [testTxt, testTxt2]
    -- forM_ (getArrayResponse ar) $ \ti -> do
    --     print ti
    --     putStrLn "\n"
    -- pure ar



testTxt :: Text
testTxt =
    "USA:s president Donald Trumps går till förnyat angrepp på Londons borgmästare Sadiq Khan \
    \– för andra gången inom två dygn efter terrordådet i den brittiska huvudstaden. \
    \Storbritanniens premiärminister har nu ställt upp till försvar för borgmästaren. Liksom USA:s \
    \förenade borgmästare."

testTxt2 :: Text
testTxt2 =
    "När ärkekonservativa DUP ställer upp som stödparti till den konservativa regeringen kommer \
    \det nordirländska partiet med största sannolikhet att kräva en ”mjukare Brexit” än vad Theresa \
    \May hittills förespråkat."
