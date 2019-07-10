{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Microsoft.Translator (

    -- * Basic Types
      SubscriptionKey (..)
    , AuthToken
    , AuthData (..)
    , TransData (..)

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

    -- *** Translation
    , basicTranslate
    , translate

) where

import           Microsoft.Translator.API
import           Microsoft.Translator.API.Auth
import           Microsoft.Translator.Exception
import           Microsoft.Translator.Language

import           Control.Concurrent             (forkIO, threadDelay, ThreadId)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.IORef
import           Data.String                    (fromString)
import           Data.Time
import           Data.Text (Text)
import           Data.Functor ((<&>))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment             (lookupEnv)


-- | Retrieve your subscription key from the TRANSLATOR_SUBSCRIPTION_KEY environment
--   variable.
lookupSubKey :: IO (Either TranslatorException SubscriptionKey)
lookupSubKey
    = fmap (maybe (Left MissingSubscriptionKey) (Right . SubKey . fromString))
    $ lookupEnv "TRANSLATOR_SUBSCRIPTION_KEY"


-- | An 'AuthToken' together with the time it was recieved.
--   Each token is valid for 10 minutes.
data AuthData = AuthData
    { timeStamp :: UTCTime
    , authToken :: AuthToken
    } deriving Show


-- | Retrieve a token, via 'issueToken', and save it together with a timestamp.
issueAuth :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthData)
issueAuth man key = do
    mTok <- issueToken man key
    now <- getCurrentTime
    pure $ mTok <&> \tok -> AuthData now tok


-- | The data to hold onto for making translation requests.
--   Includes your 'SubscriptionKey', an `AuthData` and an HTTPS 'Manager'.
data TransData = TransData
    { subscriptionKey :: SubscriptionKey
    , manager         :: Manager
    , authDataRef     :: IORef AuthData
    }

-- | Retrieve an 'AuthData' token and hold on to the new HTTPS manager.
initTransData :: IO (Either TranslatorException TransData)
initTransData = runExceptT $ do
    subKey <- ExceptT lookupSubKey
    man <- liftIO (newManager tlsManagerSettings)
    ExceptT $ initTransDataWith subKey man

-- | Retrieve an 'AuthData' token and hold on to the HTTPS manager.
--   For when you want to supply a particular manager. Otherwise use 'initTransData'.
initTransDataWith :: SubscriptionKey -> Manager -> IO (Either TranslatorException TransData)
initTransDataWith key man
    = runExceptT
    . fmap (TransData key man)
    $ ExceptT (issueAuth man key) >>= liftIO . newIORef


refresh :: TransData -> IO (Either TranslatorException AuthData)
refresh tdata = runExceptT $ do
    auth <- ExceptT $ issueAuth (manager tdata) (subscriptionKey tdata)
    liftIO $ writeIORef (authDataRef tdata) auth
    pure auth

-- | If a token contained in a 'TransData' is expired or about to expire, refresh it.
checkAuth :: TransData -> IO (Either TranslatorException AuthData)
checkAuth tdata = do
    now <- getCurrentTime
    auth <- readIORef $ authDataRef tdata
    if (diffUTCTime now (timeStamp auth) > 9*60+30)
        then refresh tdata
        else pure $ Right auth

-- | Create a 'TransData' with a new auth token and fork a thread to refresh it every
--   9 minutes. You specify what to do if the forked thread encounteres an exception.
keepFreshAuth :: (TransData -> TranslatorException -> IO ())
              -> IO (Either TranslatorException (TransData, ThreadId))
keepFreshAuth onLoopError = runExceptT $ do
    transData <- ExceptT initTransData
    threadId <- liftIO . forkIO $ loop transData
    pure (transData, threadId)

    where
        loop :: TransData -> IO ()
        loop td = do
            threadDelay $ 10^(6::Int) * 9 * 60
            _ <- either (onLoopError td) (const $ pure ()) =<< refresh td
            loop td

translate :: TransData -> Maybe Language -> Language -> [Text]
          -> IO (Either TranslatorException [TransResponse])
translate tdata mFromLang toLang txt = runExceptT $ do
     tok <- authToken <$> ExceptT (checkAuth tdata)
     ExceptT $
        bimap APIException id <$> basicTranslate (manager tdata) tok mFromLang toLang txt
