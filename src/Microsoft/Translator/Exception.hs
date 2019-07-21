{-# LANGUAGE DeriveDataTypeable #-}

module Microsoft.Translator.Exception where

import           Control.Exception
import           Data.Text
import           Data.Typeable
import           Servant.Client
import           Text.XML.Light.Types


data TranslatorException
    = APIException ServantError
    | InvalidXML Text
    | UnexpectedXMLLayout Element
    | MissingSubscriptionKey
    deriving (Show, Typeable)

instance Exception TranslatorException
