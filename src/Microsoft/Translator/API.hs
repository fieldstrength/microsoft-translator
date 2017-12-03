{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Microsoft.Translator.API (

      basicTranslate
    , basicTranslateArray
    , TranslatorException
    , Language (..)
    , ArrayRequest (..)
    , ArrayResponse (..)
    , TransItem (..)

) where

import           Microsoft.Translator.API.Auth
import           Microsoft.Translator.Exception
import           Microsoft.Translator.Language

import           Data.Bifunctor
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Monoid
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


baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.microsofttranslator.com" 443 "/V2/Http.svc"

-- | MS Microsoft.Translator API
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
        :> ReqBody '[XML] ArrayRequest
        :> Post    '[XML] ArrayResponse


newtype TransText = TransText { getTransText :: Text }
    deriving Generic

data ArrayRequest = ArrayRequest
    { fromLang :: Language
    , toLang   :: Language
    , texts    :: [Text]
    }

newtype ArrayResponse = ArrayResponse
    { getArrayResponse :: [TransItem] }
    deriving (Show,Generic)

data TransItem = TransItem
    { transText        :: Text
    , originalBreaks   :: [Int]
    , translatedBreaks :: [Int]
    } deriving (Show, Generic)


-- | JSON Web Token content type
data XML
    deriving Typeable

instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance MimeUnrender XML TransText where
    mimeUnrender _ bs = first show $ do
        txt <- first InvalidUTF8 . decodeUtf8' $ toStrict bs
        el <- maybe (Left $ InvalidXML txt) Right $ parseXMLDoc txt
        maybe (Left $ UnexpectedXMLLayout el) Right $ extractTransResponse el

extractTransResponse :: Element -> Maybe TransText
extractTransResponse
    = fmap (TransText . pack . cdData)
    . headMay
    . onlyText
    . elContent

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
            str <> "</string>"

instance MimeRender XML ArrayRequest where
    mimeRender _ = fromStrict . encodeUtf8 . encodeRequestXML

instance MimeUnrender XML ArrayResponse where
    mimeUnrender _ bs = first show $ do
        txt <- first InvalidUTF8 . decodeUtf8' $ toStrict bs
        el <- maybe (Left $ InvalidXML txt) Right $ parseXMLDoc txt
        extractArrayResponse el

extractArrayResponse :: Element -> Either TranslatorException ArrayResponse
extractArrayResponse
    = fmap ArrayResponse
    . traverse extractItem
    . onlyElems
    . elContent

extractItem :: Element -> Either TranslatorException TransItem
extractItem el@(onlyElems . elContent -> l) =
    maybe (Left $ UnexpectedXMLLayout el) Right $ do
        txtElem <- headMay [ x | x <- l, qName (elName x) == "TranslatedText" ]
        origSep <- extractBreaks =<<
            headMay [ x | x <- l, qName (elName x) == "OriginalTextSentenceLengths" ]
        transSep <- extractBreaks =<<
            headMay [ x | x <- l, qName (elName x) == "TranslatedTextSentenceLengths" ]
        cd <- headMay . onlyText  $ elContent txtElem
        Just $ TransItem (pack $ cdData cd) origSep transSep

extractBreaks :: Element -> Maybe [Int]
extractBreaks
    = traverse readMay
    . fmap cdData
    . onlyText
    . concatMap elContent
    . onlyElems
    . elContent


transClient :: Maybe AuthToken -> Maybe Text -> Maybe Language -> Maybe Language -> ClientM TransText
arrayClient :: Maybe AuthToken -> ArrayRequest -> ClientM ArrayResponse
transClient :<|> arrayClient = client (Proxy @ API)

basicTranslate :: Manager -> AuthToken -> Maybe Language -> Language -> Text
               -> IO (Either TranslatorException Text)
basicTranslate man tok from to txt =
    bimap APIException getTransText <$>
        runClientM
            (transClient (Just tok) (Just txt) from (Just to))
            (ClientEnv man baseUrl)

basicTranslateArray :: Manager -> AuthToken -> Language -> Language -> [Text]
                    -> IO (Either TranslatorException ArrayResponse)
basicTranslateArray man tok from to txts =
    bimap APIException id <$>
        runClientM
            (arrayClient (Just tok) (ArrayRequest from to txts))
            (ClientEnv man baseUrl)
