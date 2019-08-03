{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | Servant types and client for the authorization API
module Microsoft.Translator.API.Auth (

      SubscriptionKey (..)
    , TranslatorException (..)
    , AuthToken
    , issueToken

) where

import           Microsoft.Translator.Exception

import           Control.Arrow        (left)
import           Data.Bifunctor
import           Data.ByteString.Lazy (toStrict)
import           Data.String
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8')
import           Data.Typeable
import           GHC.Generics         (Generic)
import           Network.HTTP.Client  hiding (Proxy)
import qualified Network.HTTP.Media   as M
import           Servant.API
import           Servant.Client


authUrl :: BaseUrl
authUrl = BaseUrl Https "api.cognitive.microsoft.com" 443 "/sts/v1.0"

-- | MS Translator token service API
type AuthAPI =
    "issueToken"
        :> QueryParam "Subscription-Key" SubscriptionKey
        :> Post '[JWT] AuthToken

-- | A key to your subscription to the service. Used to retrieve an 'AuthToken'.
--   Put it in the environment variable @TRANSLATOR_SUBSCRIPTION_KEY@.
newtype SubscriptionKey
    = SubKey Text
    deriving (Show, ToHttpApiData, IsString)

-- | The JSON Web Token issued by MS Translator token service. Consists of wrapped text.
--   Valid for ten minutes.
newtype AuthToken
    = AuthToken Text
    deriving (Show, Generic)

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
issueToken man key = first APIException <$>
    runClientM (authClient $ Just key) (ClientEnv man authUrl Nothing)
