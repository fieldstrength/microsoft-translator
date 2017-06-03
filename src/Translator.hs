{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Translator (

      SubscriptionKey
    , AuthToken

    , Language (..)

    , TranslatorException

    , issueToken
    , translate
    , simpleTranslate

) where

import           Translator.API.Auth.Client
import           Translator.API.Auth.Types
import           Translator.API.Client
import           Translator.API.Types
import           Translator.Exception
import           Translator.Language

import           Control.Exception
import           Data.Text
import           Network.HTTP.Client

simpleTranslate :: SubscriptionKey -> Manager
                -> Maybe Language -> Language -> Text -> IO (Either TranslatorException Text)
simpleTranslate key man from to txt =
    try $ do
        tok <- issueToken man key     >>= either throw pure
        translate man tok from to txt >>= either throw pure
