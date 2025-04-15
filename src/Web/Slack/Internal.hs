-- Due to AuthClientData
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal things in slack-web. May be changed arbitrarily!
module Web.Slack.Internal where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Network.HTTP.Client (Manager)
import Servant.API hiding (addHeader)
import Servant.Client (BaseUrl (..), ClientError, ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)
import Web.Slack.Common qualified as Common
import Web.Slack.Pager (Response)
import Web.Slack.Prelude

data SlackConfig = SlackConfig
  { slackConfigManager :: Manager
  , slackConfigToken :: Text
  }

-- |
-- Internal type!
newtype ResponseJSON a = ResponseJSON (Either Common.ResponseSlackError a)
  deriving stock (Show)

type role ResponseJSON representational

instance (FromJSON a) => FromJSON (ResponseJSON a) where
  parseJSON = withObject "Response" $ \o -> do
    ok <- o .: "ok"
    ResponseJSON
      <$> if ok
        then Right <$> parseJSON (Object o)
        else do
          err <- o .: "error"
          meta <- o .:? "response_metadata"
          pure $ Left $ Common.ResponseSlackError {errorText = err, responseMetadata = (fromMaybe KM.empty meta)}

mkSlackAuthenticateReq :: SlackConfig -> AuthenticatedRequest (AuthProtect "token")
mkSlackAuthenticateReq = (`mkAuthenticatedRequest` authenticateReq) . slackConfigToken

type instance
  AuthClientData (AuthProtect "token") =
    Text

authenticateReq ::
  Text ->
  Request ->
  Request
authenticateReq token =
  addHeader "Authorization" $ "Bearer " <> token

run ::
  ClientM (ResponseJSON a) ->
  Manager ->
  IO (Response a)
run clientAction mgr = do
  let baseUrl = BaseUrl Https "slack.com" 443 "/api"
  unnestErrors <$> liftIO (runClientM clientAction $ mkClientEnv mgr baseUrl)

unnestErrors :: Either ClientError (ResponseJSON a) -> Response a
unnestErrors (Right (ResponseJSON (Right a))) = Right a
unnestErrors (Right (ResponseJSON (Left err))) =
  Left (Common.SlackError err)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)
