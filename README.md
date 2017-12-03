## Microsoft Translator

Bindings to the text portion of the [Microsoft Translator API](https://www.microsoft.com/en-us/translator/products.aspx).

The service has a free tier allowing for 2 million translated characters per month.

The supported endpoints allow for translating (lists of) text, and provide sentence-break information about both the original and returned passages.

#### Overview

The Translator API is really two APIs.

* An authorization API where you exchange one of your subscription keys for a JWT auth token.
* The translation API itself which takes the JWTs for authentication.

The basic aim of the library is to abstract away the work of handling the authorization and allow the user to just say what they want translated.

A design principle is to represent error modes in the types. Thus we provide functions with return types of the form `IO (Either TranslatorException a)`. They are defined in terms of `ExceptT` variants, which allow the library to be written more easily, by handling the composition of these functions with the appropriate error semantics. These are also exported.

A basic use-case proceeds like this: We start with a `SubscriptionKey`, generally by reading the `TRANSLATOR_SUBSCRIPTION_KEY` environment variable. When we retrieve our JWT auth token, we put it in an `AuthData`
which also contains the time it was received. This is in turn included (in the form of an `IORef`) in a `TransData`, which includes the `Manager` and the `SubscriptionKey`. The `TransData` is then all that needs to be passed to each translation function, each of which will if necessary refresh the authorization. Its also possible to set up a loop to continually refresh the token so that it's always ready.

#### Example

```haskell
import           Microsoft.Translator
import           Control.Monad.Except
import qualified Data.Text.IO as T

main :: IO ()
main = do
    -- set your subscription key in the TRANSLATOR_SUBSCRIPTION_KEY environment var
    Right transData <- runExceptT (lookupSubKey >>= initTransData)
    forever $ do
        T.putStr "> "
        str <- T.getLine
        Right txt <- translateIO transData Nothing ChineseTraditional str
        T.putStrLn txt
        T.putStrLn ""
```

```
> already you're the monarch of your own skin
你已經是你自己皮膚的君主

> your inviolable freedom waits to be completed only by the love of other monarchs
你神聖的自由只等待著其他君主的愛來完成

> a politics of dream, urgent as the blueness of sky
夢想的政治, 緊迫的天空的蔚藍
```
