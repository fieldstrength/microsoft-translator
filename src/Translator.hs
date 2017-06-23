module Translator (

    -- * Basic Types
      SubscriptionKey
    , AuthToken
    , AuthData (..)
    , TransData

    , Language (..)
    , TranslatorException

    , ArrayResponse (..)
    , TransItem (..)

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

    -- *** IO variants
    , issueAuthIO
    , initTransDataIO
    , checkTransDataIO
    , translateIO
    , translateArrayIO

    -- *** Minimalistic variants
    , simpleTranslate
    , basicTranslate
    , basicTranslateArray

) where

import           Translator.API
import           Translator.API.Auth

import           Control.Exception
import           Control.Monad.Except
import           Data.Text
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
data AuthData
    = AuthData
        { timeStamp :: UTCTime
        , authToken :: AuthToken
        }
    deriving Show

-- | Retrieve a token, as in 'issueToken' and save it together with a timestamp.
issueAuth :: Manager -> SubscriptionKey -> ExceptT TranslatorException IO AuthData
issueAuth man key = do
    tok <- ExceptT $ issueToken man key
    now <- liftIO getCurrentTime
    pure $ AuthData now tok

-- | The data to hold onto for making translation requests.
--   Includes your 'SubscriptionKey', an `AuthData` and an HTTPS 'Manager'.
data TransData
    = TransData
        { subKey   :: SubscriptionKey
        , manager  :: Manager
        , authData :: AuthData
        }

-- | Retrieve an 'AuthData' token and start up an HTTPS manager.
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
    auth <- case (diffUTCTime now before > 9*60) of
        True  -> issueAuth (manager tdata) (subKey tdata)
        False -> pure (authData tdata)
    pure $ tdata { authData = auth }

-- | Translate text
translate :: TransData -> Maybe Language -> Language -> Text
          -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     td <- checkTransData tdata
     ExceptT $ basicTranslate (manager td) (authToken $ authData td) from to txt

-- | Translate text array
translateArray :: TransData -> Language -> Language -> [Text]
               -> ExceptT TranslatorException IO ArrayResponse
translateArray tdata from to txts = do
     td <- checkTransData tdata
     ExceptT $ basicTranslateArray (manager td) (authToken $ authData td) from to txts


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
