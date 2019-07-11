{-# LANGUAGE OverloadedStrings #-}

module Microsoft.TranslatorExample where

import           Microsoft.Translator

import           Control.Monad (forever)
import qualified Data.Text.IO as T


main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- initTransData
    forever $ do
        T.putStr "\n> "
        str <- T.getLine
        mtxt <- translate transData Nothing Swedish True [str]
        print mtxt
