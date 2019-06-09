{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# options -w #-}

module Microsoft.Translator (

    -- * Basic Types
      SubscriptionKey (..)
    , AuthToken
    , AuthData (..)
    , TransData

    , Language (..)
    , TranslatorException (..)

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
    --, translate

    -- *** IO variants
    , lookupSubKeyIO
    , issueAuthIO
    , initTransDataIO
    , checkAuthIO
    --, translateIO

    -- *** Minimalistic variants
    --, simpleTranslate
    , basicTranslate

) where

import           Microsoft.Translator.API
import           Microsoft.Translator.API.Auth
import           Microsoft.Translator.Exception
import           Microsoft.Translator.Language

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.IORef
import           Data.String                    (fromString)
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment             (lookupEnv)


---- | Simplest possible translation function.
----   Always needs to make a request for the JWT token first.
--simpleTranslate :: SubscriptionKey -> Manager
--                -> Maybe Language -> Language
--                -> Text -> IO (Either TranslatorException Text)
--simpleTranslate key man from to txt = runExceptT $ do
--        tok <- ExceptT $ issueToken man key
--        ExceptT $ basicTranslate man tok from to txt

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
    , authDataRef :: IORef AuthData
    }

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

foo :: IO ()
foo = do
    r <- runExceptT $ do
        sk <- lookupSubKey
        tdata <- initTransData sk
        tok <- fmap authToken . liftIO . readIORef $ authDataRef tdata
        ExceptT . fmap (bimap APIException id) $
            basicTranslate (manager tdata) tok Nothing English
                ["Översätta mig nu", "Jag kan prata Svenska"]
    print r

---- | Translate text
--translate :: TransData -> Maybe Language -> Language -> Text
--          -> ExceptT TranslatorException IO Text
--translate tdata from to txt = do
--     tok <- authToken <$> checkAuth tdata
--     ExceptT $ V3.basicTranslate (manager tdata) tok from to txt


-- | Retrieve a token, via 'issueToken', and save it together with a timestamp.
issueAuthIO :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthData)
issueAuthIO man = runExceptT . issueAuth man

-- | Retrieve an 'AuthData' token and start up an HTTPS manager.
initTransDataIO :: SubscriptionKey -> IO (Either TranslatorException TransData)
initTransDataIO = runExceptT . initTransData

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkAuthIO :: TransData -> IO (Either TranslatorException AuthData)
checkAuthIO = runExceptT . checkAuth
