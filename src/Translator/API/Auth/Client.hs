{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Translator.API.Auth.Client where

import           Data.Proxy
import           Data.Text                 (Text)
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           Translator.API.Auth.Types


authClient :: Maybe SubscriptionKey -> ClientM AuthToken
authClient = client (Proxy @ AuthAPI)

issueToken :: Manager -> SubscriptionKey -> IO (Either ServantError AuthToken)
issueToken man key = runClientM (authClient $ Just key) (ClientEnv man baseUrl)
