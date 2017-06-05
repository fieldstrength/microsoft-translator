{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Translator.API (

      translateIO
    , translateArrayIO
    , TranslatorException
    , Language (..)
    , ArrayRequest (..)
    , ArrayResponse

) where

import           Translator.API.Auth
import           Translator.Exception
import           Translator.Language

import           Data.Bifunctor
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Monoid
import           Data.Proxy

import           Data.Text            as T (Text, stripPrefix, stripSuffix, unlines)
import           Data.Text.Encoding   (decodeUtf8', encodeUtf8)
import           Data.Typeable
import           Network.HTTP.Client  hiding (Proxy)
import qualified Network.HTTP.Media   as M
import           Servant.API
import           Servant.Client

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.microsofttranslator.com" 443 "/V2/Http.svc"

-- | MS Translator API
--   http://docs.microsofttranslator.com/text-translate.html#!/default/get_Translate
type API =
    "Translate"
        :> Header "authorization" AuthToken
        :> QueryParam "text"  Text
        :> QueryParam "from"  Language
        :> QueryParam "to"    Language
        :> Get '[XML] TransText
    :<|> "TranslateArray"
        :> Header "authorization" AuthToken
        :> ReqBody '[XML]         ArrayRequest
        :> Post '[XML]            ArrayResponse


newtype TransText
    = TransText { getTransText :: Text }

data ArrayRequest
    = ArrayRequest
        { fromLang :: Language
        , toLang   :: Language
        , texts    :: [Text]
        }

encodeRequestXML :: ArrayRequest -> Text
encodeRequestXML (ArrayRequest from to txts) = T.unlines $
    [ "<TranslateArrayRequest>"
    , "  <AppId />"
    , "  <From>" <> toLangCode from <> "</From>"
    , "  <Texts>"
    ] <> fmap xmlString txts <>
    [ "  </Texts>"
    , "  <To>" <> toLangCode to <> "</To>"
    , "</TranslateArrayRequest>"
    ]

    where
        xmlString str =
            "<string xmlns=\"http://schemas.microsoft.com/2003/10/Serialization/Arrays\">" <>
            str <>
            "</string>"

newtype ArrayResponse
    = ArrayResponse { getArrayResponse :: Text }
    deriving Show

-- | JSON Web Token content type
data XML
    deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance MimeUnrender XML Text where
    mimeUnrender _ = first show . decodeUtf8' . toStrict

instance MimeUnrender XML TransText where
    mimeUnrender _ bs = do
        txt <- first show . decodeUtf8' $ toStrict bs
        t1 <- maybe (Left $ "Unexpected prefix: " <> show txt) Right $
            stripPrefix "<string xmlns=\"http://schemas.microsoft.com/2003/10/Serialization/\">" txt
        t2 <- maybe (Left $ "Unexpected suffix: " <> show txt) Right $
            stripSuffix "</string>" t1
        pure (TransText t2)

instance MimeRender XML ArrayRequest where
    mimeRender _ = fromStrict . encodeUtf8 . encodeRequestXML

instance MimeUnrender XML ArrayResponse where
    mimeUnrender _ bs = do
        txt <- first show . decodeUtf8' $ toStrict bs
        pure (ArrayResponse txt)


transClient :: Maybe AuthToken -> Maybe Text -> Maybe Language -> Maybe Language -> ClientM TransText
arrayClient :: Maybe AuthToken -> ArrayRequest -> ClientM ArrayResponse
transClient :<|> arrayClient = client (Proxy @ API)

translateIO :: Manager -> AuthToken -> Maybe Language -> Language -> Text
            -> IO (Either TranslatorException Text)
translateIO man tok from to txt = bimap TranslatorException getTransText <$>
    runClientM (transClient (Just tok) (Just txt) from (Just to)) (ClientEnv man baseUrl)

translateArrayIO :: Manager -> AuthToken -> Language -> Language -> [Text]
                 -> IO (Either TranslatorException Text)
translateArrayIO man tok from to txts =
    bimap TranslatorException getArrayResponse <$>
        runClientM
            (arrayClient (Just tok) (ArrayRequest from to txts))
            (ClientEnv man baseUrl)
