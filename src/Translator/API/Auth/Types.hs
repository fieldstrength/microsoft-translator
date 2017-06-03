{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Translator.API.Auth.Types where

import           Control.Arrow        (left)
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8')
import           Data.Typeable
import qualified Network.HTTP.Media   as M
import           Servant.API
import           Servant.Client


baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.cognitive.microsoft.com" 443 "/sts/v1.0"

-- | MS Translator token service API
--   http://docs.microsofttranslator.com/oauth-token.html
type AuthAPI =
    "issueToken"
        :> QueryParam "Subscription-Key" SubscriptionKey
        :> Post '[JWT] AuthToken

type SubscriptionKey = Text

newtype AuthToken
    = AuthToken Text
    deriving Show

-- | JSON Web Token content type
data JWT
    deriving Typeable

instance Accept JWT where
    contentType _ = "application" M.// "jwt" M./: ("charset", "us-ascii")

instance MimeUnrender JWT AuthToken where
    mimeUnrender _ = fmap AuthToken . left show . decodeUtf8' . toStrict

instance ToHttpApiData AuthToken where
    toUrlPiece (AuthToken txt) = "Bearer " <> txt
