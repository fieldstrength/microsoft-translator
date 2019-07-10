{-# LANGUAGE OverloadedStrings #-}
{-# options -w #-}

module Microsoft.TranslatorExample where

import           Microsoft.Translator

import           Control.Monad.Except
import qualified Data.Text.IO as T


main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- runExceptT $ ExceptT lookupSubKey >>= ExceptT . initTransData
    forever $ do
        T.putStr "\n> "
        str <- T.getLine
        mtxt <- translate transData Nothing Swedish [str]
        print mtxt
