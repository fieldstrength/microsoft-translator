{-# LANGUAGE DeriveDataTypeable #-}

module Translator.Exception where

import           Control.Exception
import           Data.Typeable
import           Servant.Client

newtype TranslatorException
    = TranslatorException ServantError
    deriving (Show, Typeable)

instance Exception TranslatorException
