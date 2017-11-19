{-# LANGUAGE OverloadedStrings #-}

module TranslatorExample where

import           Translator

import           Control.Monad.Except
import qualified Data.Text.IO as T


main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- runExceptT (lookupSubKey >>= initTransData)
    forever $ do
        T.putStrLn "Tell me a story: "
        str <- T.getLine
        Right txt <- translateIO transData Nothing ChineseTraditional str
        T.putStrLn txt
