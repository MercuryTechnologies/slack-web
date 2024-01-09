-- | Internal things in slack-web. May be changed arbitrarily!
module Web.Slack.Internal where

import Data.Aeson (Value (..))
import Network.HTTP.Client (Manager)
import Servant.API hiding (addHeader)
-- import Servant.Client.Core

import Servant.Client (BaseUrl (..), ClientError, ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)
import Web.Slack.Common qualified as Common
import Web.Slack.Pager (Response)
import Web.Slack.Prelude

data SlackConfig = SlackConfig
  { slackConfigManager :: Manager
  , slackConfigToken :: Text
  }

-- contains errors that can be returned by the slack API.
-- constrast with 'SlackClientError' which additionally
-- contains errors which occured during the network communication.
data ResponseSlackError = ResponseSlackError Text
  deriving stock (Eq, Show)

-- |
-- Internal type!
newtype ResponseJSON a = ResponseJSON (Either ResponseSlackError a)

instance (FromJSON a) => FromJSON (ResponseJSON a) where
  parseJSON = withObject "Response" $ \o -> do
    ok <- o .: "ok"
    ResponseJSON
      <$> if ok
        then Right <$> parseJSON (Object o)
        else Left . ResponseSlackError <$> o .: "error"

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
unnestErrors (Right (ResponseJSON (Left (ResponseSlackError serv)))) =
  Left (Common.SlackError serv)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)
