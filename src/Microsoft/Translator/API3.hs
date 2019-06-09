{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-unused-imports          #-}

-- | Servant types and client for the API
module Microsoft.Translator.API3 where

import           Microsoft.Translator.API.Auth
import           Microsoft.Translator.Exception
import           Microsoft.Translator.Language

import Data.Aeson
import           Data.Bifunctor
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Proxy
import           Data.Text            as T (Text, pack, unlines)
import           Data.Text.Encoding   (decodeUtf8', encodeUtf8)
import           Data.Typeable
import           GHC.Generics         (Generic)
import           Network.HTTP.Client  hiding (Proxy)
import qualified Network.HTTP.Media   as M
import           Safe                 (headMay, readMay)
import           Servant.API
import           Servant.Client
import           Text.XML.Light.Input
import           Text.XML.Light.Proc
import           Text.XML.Light.Types


apiBaseUrl :: BaseUrl
apiBaseUrl = BaseUrl Https "api.cognitive.microsofttranslator.com" 443 ""

-- | MS Microsoft.Translator API
--   https://docs.microsoft.com/en-us/azure/cognitive-services/translator/migrate-to-v3
--   https://docs.microsoft.com/en-us/azure/cognitive-services/translator/reference/v3-0-translate?tabs=curl
type API =
    "Translate"
        :> Header "authorization" AuthToken
        :> QueryParam "api-version" Text
        :> QueryParam "from"  Language  -- optional
        :> QueryParam "to"    Language
        :> ReqBody '[JSON] [TransItem]
        :> Post '[JSON] [TransResponse]

data TransItem = TransItem { itemText :: Text } deriving (Show, Generic)

instance ToJSON TransItem where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = const "text"}



data Translations = Translations
    { to   :: Text
    , text :: Text
    } deriving (Show, Generic, FromJSON)

data TransResponse = TransResponse
    { translations :: [Translations]
--  , detectedLanguage :: { language :: Text, score :: Number}
    } deriving (Show, Generic, FromJSON)

transClient :: Maybe AuthToken -> Maybe Text
            -> Maybe Language -> Maybe Language -> [TransItem] -> ClientM [TransResponse]
transClient = client (Proxy @ API)

-- | Most basic possible text translation function. For typical use-cases it will be much
--   more convenient to use functions from the "Microsoft.Translator" module, namely
--   'Microsoft.Translator.translateIO'. See the README example.
basicTranslate :: Manager -> AuthToken -> Maybe Language -> Language -> [Text]
               -> IO (Either TranslatorException [TransResponse])
basicTranslate man tok fromLang toLang txts =
    bimap APIException id <$>
        runClientM
            (transClient (Just tok) (Just "3.0") fromLang (Just toLang) (TransItem <$> txts))
            (ClientEnv man apiBaseUrl Nothing)
