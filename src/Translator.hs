{-# LANGUAGE OverloadedStrings     #-}

module Translator (

      SubscriptionKey
    , AuthToken
    , AuthData (..)
    , TransData (..)

    , Language (..)
    , TranslatorException

    , issueToken
    , issueAuth
    , initTransData
    , checkTransData
    , translate
    , translateArray

    , translateIO
    , translateArrayIO
    , simpleTranslate

) where

import           Translator.API
import           Translator.API.Auth

import           Control.Exception
import           Control.Monad.Except
import           Data.Text
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

simpleTranslate :: SubscriptionKey -> Manager
                -> Maybe Language -> Language -> Text -> IO (Either TranslatorException Text)
simpleTranslate key man from to txt =
    try $ do
        tok <- issueToken man key       >>= either throw pure
        translateIO man tok from to txt >>= either throw pure


-- | An 'AuthToken' together with the time it was recieved. Each token is valid for 10 minutes.
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

data TransData
    = TransData
        { subKey   :: SubscriptionKey
        , manager  :: Manager
        , authData :: AuthData
        }

-- | Retrieve a token 'AuthData' and start up an Https manager.
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
translate :: TransData -> Maybe Language -> Language -> Text -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     td <- checkTransData tdata
     ExceptT $ translateIO (manager td) (authToken $ authData td) from to txt

-- | Translate text array
translateArray :: TransData -> Language -> Language -> [Text] -> ExceptT TranslatorException IO Text
translateArray tdata from to txts = do
     td <- checkTransData tdata
     ExceptT $ translateArrayIO (manager td) (authToken $ authData td) from to txts
