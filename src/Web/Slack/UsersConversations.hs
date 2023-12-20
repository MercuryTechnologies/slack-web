{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Client for Slack's @users.conversations@ endpoint.
-- <https://api.slack.com/methods/users.conversations>
module Web.Slack.UsersConversations (
  UsersConversationsRequest (..),
  UsersConversationsResponse (..),
  usersConversations,
  usersConversationsAll,
) where

import Servant.API
import Servant.Client hiding (Response)
import Servant.Client.Core hiding (Response)
import Web.FormUrlEncoded (ToForm (..), genericToForm)
import Web.Slack.AesonUtils
import Web.Slack.Common (Cursor, TeamId, UserId)
import Web.Slack.Conversation (Conversation, ConversationType)
import Web.Slack.Internal
import Web.Slack.Pager (LoadPage, PagedRequest (..), PagedResponse (..), Response, ResponseMetadata, fetchAllBy)
import Web.Slack.Prelude

newtype ConversationTypesList = ConversationTypesList {unConversationTypesList :: NonEmpty ConversationType}
  deriving stock (Show, Eq)
  deriving newtype (NFData)

instance ToHttpApiData ConversationTypesList where
  toQueryParam (ConversationTypesList list) = intercalate "," $ toUrlPiece <$> list

-- | <https://api.slack.com/methods/users.conversations>
data UsersConversationsRequest = UsersConversationsRequest
  { cursor :: Maybe Cursor
  , excludeArchived :: Maybe Bool
  , limit :: Maybe Int
  , teamId :: Maybe TeamId
  , types :: Maybe ConversationTypesList
  -- ^ Specify some specific conversation types. Defaults on Slack's end to
  -- @public_channel@, aka 'PublicChannelType'.
  , user :: Maybe UserId
  -- ^ Look at the conversations of a specified user rather than the calling
  -- user
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Default)

instance ToForm UsersConversationsRequest where
  toForm = genericToForm snakeCaseFormOptions

instance PagedRequest UsersConversationsRequest where
  setCursor c req = req {cursor = c}

data UsersConversationsResponse = UsersConversationsResponse
  { channels :: [Conversation]
  , responseMetadata :: Maybe ResponseMetadata
  }
  deriving stock (Show, Eq)

instance PagedResponse UsersConversationsResponse where
  type ResponseObject UsersConversationsResponse = Conversation
  getResponseData UsersConversationsResponse {channels} = channels
  getResponseMetadata UsersConversationsResponse {responseMetadata} = responseMetadata

$(deriveFromJSON snakeCaseOptions ''UsersConversationsResponse)

type Api =
  "users.conversations"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] UsersConversationsRequest
    :> Post '[JSON] (ResponseJSON UsersConversationsResponse)

-- | Client for Slack's @users.conversations@ endpoint.
-- <https://api.slack.com/methods/users.conversations>
usersConversations :: SlackConfig -> UsersConversationsRequest -> IO (Response UsersConversationsResponse)
usersConversations slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (usersConversations_ authR req) . slackConfigManager $ slackConfig

usersConversations_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  UsersConversationsRequest ->
  ClientM (ResponseJSON UsersConversationsResponse)
usersConversations_ = client (Proxy @Api)

-- | Fetch all user conversations.
usersConversationsAll :: SlackConfig -> UsersConversationsRequest -> IO (LoadPage IO Conversation)
usersConversationsAll = fetchAllBy . usersConversations
