## Microsoft Translator

Bindings to the text portion of the [Microsoft Translator API](https://www.microsoft.com/en-us/translator/products.aspx).

The service has a free tier allowing for 2 million translated characters per month.

#### Example

```haskell
import           Translator
import           Control.Monad.Except
import qualified Data.Text.IO as T

main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- runExceptT (lookupSubKey >>= keepFreshAuth)
    forever $ do
        T.putStrLn "Tell me a story: "
        str <- T.getLine
        Right txt <- translateIO transData Nothing ChineseTraditional str
        T.putStrLn txt
```
