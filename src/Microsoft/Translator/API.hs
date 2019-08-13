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
        :> RequiredQueryParam "api-version" Text
        :> QueryParam "from" Language  -- optional
        :> RequiredQueryParam "to"   Language
        :> QueryParam "includeSentenceLength" IncludeSentenceLengths  -- optional
        :> ReqBody '[JSON] [TranslationItem]
        :> Post '[JSON] [TranslationResponse]

type RequiredQueryParam = QueryParam' '[Required,Strict]

type SourceText = Text
type TranslatedText = Text
type IncludeSentenceLengths = Bool

data TranslationItem = TranslationItem
    { itemText :: SourceText
    } deriving (Show, Generic)

instance ToJSON TranslationItem where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = const "text"}



data TranslationItemResult = TranslationItemResult
    { to      :: Language
    , text    :: TranslatedText
    , sentLen :: Maybe SentenceLengths
    } deriving (Show, Generic, FromJSON)

data TranslationResponse = TranslationResponse
    { translations :: [TranslationItemResult]
    -- ^ This is a list because the API supports specifying multiple target languages
    } deriving (Show, Generic, FromJSON)

data SentenceLengths = SentenceLengths
    { srcSentLen   :: [Int]
    , transSentLen :: [Int]
    } deriving (Show, Generic, FromJSON)

transClient :: Maybe AuthToken -> Text
            -> Maybe Language -> Language -> Maybe Bool -> [TranslationItem] -> ClientM [TranslationResponse]
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
               -> IncludeSentenceLengths -> [SourceText]
               -> IO (Either ClientError [TranslationResponse])
basicTranslate man tok fromLang toLang includeSentenceLength txts =
    runClientM
        (transClient
            (Just tok)
            "3.0"
            fromLang
            toLang
            (Just includeSentenceLength)
            (TranslationItem <$> txts)
            )
        (ClientEnv man apiBaseUrl Nothing)
