{-# LANGUAGE DeriveDataTypeable #-}

module Microsoft.Translator.Exception where

import           Control.Exception
import           Data.Typeable
import           Servant.Client


data TranslatorException
    = APIException ServantError
    | MissingSubscriptionKey
    deriving (Show, Typeable)

instance Exception TranslatorException
