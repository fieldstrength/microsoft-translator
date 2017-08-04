{-# LANGUAGE DeriveDataTypeable #-}

module Translator.Exception where

import           Control.Exception
import           Data.Text
import           Data.Text.Encoding.Error
import           Data.Typeable
import           Servant.Client
import           Text.XML.Light.Types


data TranslatorException
    = APIException ServantError
    | InvalidXML Text
    | UnexpectedXMLLayout Element
    | InvalidUTF8 UnicodeException
    deriving (Show, Typeable)

instance Exception TranslatorException
