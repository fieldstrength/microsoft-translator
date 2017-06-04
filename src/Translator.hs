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

    , translateIO
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

initTransData :: SubscriptionKey -> ExceptT TranslatorException IO TransData
initTransData key = do
    man <- liftIO $ newManager tlsManagerSettings
    auth <- issueAuth man key
    pure $ TransData key man auth

checkTransData :: TransData -> ExceptT TranslatorException IO TransData
checkTransData tdata = do
    now <- liftIO getCurrentTime
    let before = timeStamp $ authData tdata
    auth <- case (diffUTCTime now before > 9*60) of
        True  -> issueAuth (manager tdata) (subKey tdata)
        False -> pure (authData tdata)
    pure $ tdata { authData = auth }

translate :: TransData -> Maybe Language -> Language -> Text -> ExceptT TranslatorException IO Text
translate tdata from to txt = do
     td <- checkTransData tdata
     ExceptT $ translateIO (manager td) (authToken $ authData td) from to txt
