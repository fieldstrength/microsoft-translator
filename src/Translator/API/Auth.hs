{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Translator.API.Auth (

      SubscriptionKey
    , AuthToken
    , TranslatorException
    , issueToken

) where

import           Translator.Exception

import           Control.Arrow        (left)
import           Data.Bifunctor
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8')
import           Data.Typeable
import           Network.HTTP.Client  hiding (Proxy)
import qualified Network.HTTP.Media   as M
import           Servant.API
import           Servant.Client


authUrl :: BaseUrl
authUrl = BaseUrl Https "api.cognitive.microsoft.com" 443 "/sts/v1.0"

-- | MS Translator token service API
--   http://docs.microsofttranslator.com/oauth-token.html
type AuthAPI =
    "issueToken"
        :> QueryParam "Subscription-Key" SubscriptionKey
        :> Post '[JWT] AuthToken

type SubscriptionKey = Text

-- | The JSON Web Token issued by MS Translator token service. Consists of wrapped text.
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


authClient :: Maybe SubscriptionKey -> ClientM AuthToken
authClient = client (Proxy @ AuthAPI)

-- | Retrieve a token from the API. It will be valid for 10 minutes.
issueToken :: Manager -> SubscriptionKey -> IO (Either TranslatorException AuthToken)
issueToken man key = first TranslatorException <$>
    runClientM (authClient $ Just key) (ClientEnv man authUrl)
