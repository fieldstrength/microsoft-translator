{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | Servant types and client for the API
module Microsoft.Translator.API where

import           Microsoft.Translator.API.Auth
import           Microsoft.Translator.Language

import           Data.Aeson
import           Data.Proxy
import           Data.Text                      as T (Text)
import           GHC.Generics                   (Generic)
import           Network.HTTP.Client            hiding (Proxy)
import           Servant.API
import           Servant.Client


apiBaseUrl :: BaseUrl
apiBaseUrl = BaseUrl Https "api.cognitive.microsofttranslator.com" 443 ""

-- | MS Microsoft.Translator API
--   https://docs.microsoft.com/en-us/azure/cognitive-services/translator/migrate-to-v3
--   https://docs.microsoft.com/en-us/azure/cognitive-services/translator/reference/v3-0-translate?tabs=curl
type API =
    "Translate"
        :> Header "authorization" AuthToken
        :> QueryParam "api-version" Text
        :> QueryParam "from" Language  -- optional
        :> QueryParam "to"   Language
        :> QueryParam "includeSentenceLength" IncludeSentenceLengths  -- optional
        :> ReqBody '[JSON] [TransItem]
        :> Post '[JSON] [TransResponse]

type IncludeSentenceLengths = Bool

data TransItem = TransItem
    { itemText :: Text
    } deriving (Show, Generic)

instance ToJSON TransItem where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = const "text"}



data TranslationResult = TranslationResult
    { to      :: Language
    , text    :: Text
    , sentLen :: Maybe SentenceLengths
    } deriving (Show, Generic, FromJSON)

data TransResponse = TransResponse
    { translations :: [TranslationResult]
--  , detectedLanguage :: { language :: Text, score :: Number}
    } deriving (Show, Generic, FromJSON)

data SentenceLengths = SentenceLengths
    { srcSentLen   :: [Int]
    , transSentLen :: [Int]
    } deriving (Show, Generic, FromJSON)

transClient :: Maybe AuthToken -> Maybe Text
            -> Maybe Language -> Maybe Language -> Maybe Bool -> [TransItem] -> ClientM [TransResponse]
transClient = client (Proxy @ API)

-- | The most basic (though also the most general) possible text translation function.
--   For typical use-cases it will be much more convenient to use functions from the
--   "Microsoft.Translator" module, namely 'Microsoft.Translator.translate'.
--   See the README example.
--
--   The following limitations apply:
--
--     * The array can have at most 100 elements.
--     * The entire text included in the request cannot exceed 5,000 characters including spaces.
basicTranslate :: Manager -> AuthToken -> Maybe Language -> Language
               -> IncludeSentenceLengths -> [Text]
               -> IO (Either ClientError [TransResponse])
basicTranslate man tok fromLang toLang includeSentenceLength txts =
    runClientM
        (transClient
            (Just tok)
            (Just "3.0")
            fromLang
            (Just toLang)
            (Just includeSentenceLength)
            (TransItem <$> txts)
            )
        (ClientEnv man apiBaseUrl Nothing)
