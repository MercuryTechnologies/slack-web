{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack
-- Description: Provides Slack's Web API functions.
-- *Since 0.4.0.0*: The API functions is now more intuitive for newbies
-- than before. If you need compatiblity with the previous version, use
-- 'Web.Slack.Classy' instead.
module Web.Slack
  ( SlackConfig (..),
    mkSlackConfig,

    -- * Endpoints
    apiTest,
    authTest,
    chatPostMessage,
    chatUpdate,
    conversationsList,
    conversationsListAll,
    conversationsHistory,
    conversationsHistoryAll,
    conversationsReplies,
    repliesFetchAll,
    getUserDesc,
    usersList,
    userLookupByEmail,
    UsersConversations.usersConversations,
    UsersConversations.usersConversationsAll,

    -- * Requests and responses
    authenticateReq,
    Response,
    LoadPage,
  )
where

-- FIXME: Web.Slack.Prelude

import Control.Arrow ((&&&))
import Data.Map qualified as Map
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client hiding (Response, baseUrl)
import Servant.Client.Core (AuthenticatedRequest)
import Web.Slack.Api qualified as Api
import Web.Slack.Auth qualified as Auth
import Web.Slack.Chat qualified as Chat
import Web.Slack.Common qualified as Common
import Web.Slack.Conversation qualified as Conversation
import Web.Slack.Internal
import Web.Slack.Pager
import Web.Slack.User qualified as User
import Web.Slack.UsersConversations qualified as UsersConversations
import Prelude

type Api =
  "api.test"
    :> ReqBody '[FormUrlEncoded] Api.TestReq
    :> Post '[JSON] (ResponseJSON Api.TestRsp)
    :<|> "auth.test"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Auth.TestRsp)
    :<|> "conversations.list"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.ListReq
      :> Post '[JSON] (ResponseJSON Conversation.ListRsp)
    :<|> "conversations.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.HistoryReq
      :> Post '[JSON] (ResponseJSON Conversation.HistoryRsp)
    :<|> "conversations.replies"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.RepliesReq
      :> Post '[JSON] (ResponseJSON Conversation.HistoryRsp)
    :<|> "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] (ResponseJSON Chat.PostMsgRsp)
    :<|> "chat.update"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.UpdateReq
      :> Post '[JSON] (ResponseJSON Chat.UpdateRsp)
    :<|> "users.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON User.ListRsp)
    :<|> "users.lookupByEmail"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] User.Email
      :> Post '[JSON] (ResponseJSON User.UserRsp)

-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>
apiTest ::
  Manager ->
  Api.TestReq ->
  IO (Response Api.TestRsp)
apiTest mgr req = run (apiTest_ req) mgr

apiTest_ ::
  Api.TestReq ->
  ClientM (ResponseJSON Api.TestRsp)

-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>
authTest ::
  SlackConfig ->
  IO (Response Auth.TestRsp)
authTest = do
  authR <- mkSlackAuthenticateReq
  run (authTest_ authR) . slackConfigManager

authTest_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  ClientM (ResponseJSON Auth.TestRsp)

-- |
--
-- Retrieve conversations list.
--
-- <https://api.slack.com/methods/conversations.list>
conversationsList ::
  SlackConfig ->
  Conversation.ListReq ->
  IO (Response Conversation.ListRsp)
conversationsList = flip $ \listReq -> do
  authR <- mkSlackAuthenticateReq
  run (conversationsList_ authR listReq) . slackConfigManager

conversationsList_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  Conversation.ListReq ->
  ClientM (ResponseJSON Conversation.ListRsp)

-- | Returns an action to send a request to get the list of conversations in the
--   workspace.
--
--   To fetch all replies in the conversation, run the returned 'LoadPage' action
--   repeatedly until it returns an empty list.
conversationsListAll ::
  SlackConfig ->
  -- | The first request to send. _NOTE_: 'Conversation.listReqCursor' is silently ignored.
  Conversation.ListReq ->
  -- | An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
  IO (LoadPage IO Conversation.Conversation)
conversationsListAll = fetchAllBy . conversationsList

-- |
--
-- Retrieve ceonversation history.
-- Consider using 'historyFetchAll' in combination with this function.
--
-- <https://api.slack.com/methods/conversations.history>
conversationsHistory ::
  SlackConfig ->
  Conversation.HistoryReq ->
  IO (Response Conversation.HistoryRsp)
conversationsHistory = flip $ \histReq -> do
  authR <- mkSlackAuthenticateReq
  run (conversationsHistory_ authR histReq) . slackConfigManager

conversationsHistory_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  Conversation.HistoryReq ->
  ClientM (ResponseJSON Conversation.HistoryRsp)

-- |
--
-- Retrieve replies of a conversation.
-- Consider using 'repliesFetchAll' if you want to get entire replies
-- of a conversation.
--
-- <https://api.slack.com/methods/conversations.replies>
conversationsReplies ::
  SlackConfig ->
  Conversation.RepliesReq ->
  IO (Response Conversation.HistoryRsp)
conversationsReplies = flip $ \repliesReq -> do
  authR <- mkSlackAuthenticateReq
  run (conversationsReplies_ authR repliesReq) . slackConfigManager

conversationsReplies_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  Conversation.RepliesReq ->
  ClientM (ResponseJSON Conversation.HistoryRsp)

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>
chatPostMessage ::
  SlackConfig ->
  Chat.PostMsgReq ->
  IO (Response Chat.PostMsgRsp)
chatPostMessage = flip $ \postReq -> do
  authR <- mkSlackAuthenticateReq
  run (chatPostMessage_ authR postReq) . slackConfigManager

chatPostMessage_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  Chat.PostMsgReq ->
  ClientM (ResponseJSON Chat.PostMsgRsp)

-- | Updates a message.
--
-- <https://api.slack.com/methods/chat.update>
chatUpdate ::
  SlackConfig ->
  Chat.UpdateReq ->
  IO (Response Chat.UpdateRsp)
chatUpdate = flip $ \updateReq -> do
  authR <- mkSlackAuthenticateReq
  run (chatUpdate_ authR updateReq) . slackConfigManager

chatUpdate_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  Chat.UpdateReq ->
  ClientM (ResponseJSON Chat.UpdateRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>
usersList ::
  SlackConfig ->
  IO (Response User.ListRsp)
usersList = do
  authR <- mkSlackAuthenticateReq
  run (usersList_ authR) . slackConfigManager

usersList_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  ClientM (ResponseJSON User.ListRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.lookupByEmail>
userLookupByEmail ::
  SlackConfig ->
  User.Email ->
  IO (Response User.UserRsp)
userLookupByEmail = flip $ \email -> do
  authR <- mkSlackAuthenticateReq
  run (userLookupByEmail_ authR email) . slackConfigManager

userLookupByEmail_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  User.Email ->
  ClientM (ResponseJSON User.UserRsp)

-- | Returns a function to get a username from a 'Common.UserId'.
-- Comes in handy to use 'Web.Slack.MessageParser.messageToHtml'
getUserDesc ::
  -- | A function to give a default username in case the username is unknown
  (Common.UserId -> Text) ->
  -- | List of users as known by the slack server. See 'usersList'.
  User.ListRsp ->
  -- | A function from 'Common.UserId' to username.
  (Common.UserId -> Text)
getUserDesc unknownUserFn users =
  let userMap = Map.fromList $ (User.userId &&& User.userName) <$> User.listRspMembers users
   in \userId -> fromMaybe (unknownUserFn userId) $ Map.lookup userId userMap

-- | Returns an action to send a request to get the history of a conversation.
--
--   To fetch all messages in the conversation, run the returned 'LoadPage' action
--   repeatedly until it returns an empty list.
conversationsHistoryAll ::
  SlackConfig ->
  -- | The first request to send. _NOTE_: 'Conversation.historyReqCursor' is silently ignored.
  Conversation.HistoryReq ->
  -- | An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
  IO (LoadPage IO Common.Message)
conversationsHistoryAll = fetchAllBy . conversationsHistory

-- | Returns an action to send a request to get the replies of a conversation.
--
--   To fetch all replies in the conversation, run the returned 'LoadPage' action
--   repeatedly until it returns an empty list.
--
--   *NOTE*: The conversations.replies endpoint always returns the first message
--           of the thread. So every page returned by the 'LoadPage' action includes
--           the first message of the thread. You should drop it if you want to
--           collect messages in a thread without duplicates.
repliesFetchAll ::
  SlackConfig ->
  -- | The first request to send. _NOTE_: 'Conversation.repliesReqCursor' is silently ignored.
  Conversation.RepliesReq ->
  -- | An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
  IO (LoadPage IO Common.Message)
repliesFetchAll = fetchAllBy . conversationsReplies

apiTest_
  :<|> authTest_
  :<|> conversationsList_
  :<|> conversationsHistory_
  :<|> conversationsReplies_
  :<|> chatPostMessage_
  :<|> chatUpdate_
  :<|> usersList_
  :<|> userLookupByEmail_ =
    client (Proxy :: Proxy Api)

-- | Prepare a SlackConfig from a slack token.
-- You can then call the other functions providing this in a reader context.
mkSlackConfig :: Text -> IO SlackConfig
mkSlackConfig token = SlackConfig <$> newManager tlsManagerSettings <*> pure token
