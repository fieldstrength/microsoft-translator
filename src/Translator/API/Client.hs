{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Translator.API.Client where

import           Translator.API.Auth.Client
import           Translator.API.Auth.Types  (AuthToken)
import           Translator.API.Types
import           Translator.Exception
import           Translator.Language

import           Data.Bifunctor
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import           Network.HTTP.Client        hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client

transClient :: Maybe AuthToken -> Maybe Text -> Maybe Language -> Maybe Language -> ClientM TransText
transClient = client (Proxy @ API)

translate :: Manager -> AuthToken -> Maybe Language -> Language -> Text
          -> IO (Either TranslatorException Text)
translate man tok from to txt = bimap TranslatorException getTransText <$>
    runClientM (transClient (Just tok) (Just txt) from (Just to)) (ClientEnv man baseUrl)
