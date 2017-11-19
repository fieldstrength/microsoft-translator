## Microsoft Translator

Bindings to the text portion of the [Microsoft Translator API](https://www.microsoft.com/en-us/translator/products.aspx).

The service has a free tier allowing for 2 million translated characters per month.

#### Example

```haskell
main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- runExceptT $ initTransData =<< lookupSubKey
    forever $ do
        T.putStrLn "Tell me a story: "
        str <- T.getLine
        Right txt <- translateIO transData Nothing ChineseTraditional str
        T.putStrLn txt
```
