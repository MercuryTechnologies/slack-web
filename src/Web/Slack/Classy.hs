----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Classy
-- Description: For compatibility with Web.Slack prior to v0.4.0.0.
module Web.Slack.Classy (
  SlackConfig (..),
  mkSlackConfig,
  apiTest,
  authTest,
  chatPostMessage,
  conversationsList,
  conversationsHistory,
  conversationsHistoryAll,
  conversationsReplies,
  repliesFetchAll,
  getUserDesc,
  usersList,
  userLookupByEmail,
  authenticateReq,
  Response,
  LoadPage,
  HasManager (..),
  HasToken (..),
) where

import Data.Map qualified as Map
import Network.HTTP.Client (Manager)
import Web.Slack (SlackConfig (..), authenticateReq, mkSlackConfig)
import Web.Slack qualified as NonClassy
import Web.Slack.Api qualified as Api
import Web.Slack.Auth qualified as Auth
import Web.Slack.Chat qualified as Chat
import Web.Slack.Common qualified as Common
import Web.Slack.Conversation qualified as Conversation
import Web.Slack.Pager
import Web.Slack.Prelude
import Web.Slack.User qualified as User

-- | Implemented by 'SlackConfig'
class HasManager a where
  getManager :: a -> Manager

-- | Implemented by 'SlackConfig'
class HasToken a where
  getToken :: a -> Text

instance HasManager SlackConfig where
  getManager = slackConfigManager

instance HasToken SlackConfig where
  getToken = slackConfigToken

-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>
apiTest ::
  (MonadReader env m, HasManager env, MonadIO m) =>
  Api.TestReq ->
  m (Response Api.TestRsp)
apiTest = liftToReader . flip (NonClassy.apiTest . getManager)

-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>
authTest ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  m (Response Auth.TestRsp)
authTest = liftNonClassy NonClassy.authTest

-- |
--
-- Retrieve conversations list.
--
-- <https://api.slack.com/methods/conversations.list>
conversationsList ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  Conversation.ListReq ->
  m (Response Conversation.ListRsp)
conversationsList = liftNonClassy . flip NonClassy.conversationsList

-- |
--
-- Retrieve ceonversation history.
-- Consider using 'historyFetchAll' in combination with this function.
--
-- <https://api.slack.com/methods/conversations.history>
conversationsHistory ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  Conversation.HistoryReq ->
  m (Response Conversation.HistoryRsp)
conversationsHistory = liftNonClassy . flip NonClassy.conversationsHistory

-- |
--
-- Retrieve replies of a conversation.
-- Consider using 'repliesFetchAll' if you want to get entire replies
-- of a conversation.
--
-- <https://api.slack.com/methods/conversations.replies>
conversationsReplies ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  Conversation.RepliesReq ->
  m (Response Conversation.HistoryRsp)
conversationsReplies = liftNonClassy . flip NonClassy.conversationsReplies

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>
chatPostMessage ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  Chat.PostMsgReq ->
  m (Response Chat.PostMsgRsp)
chatPostMessage = liftNonClassy . flip NonClassy.chatPostMessage

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>
usersList ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  User.ListReq ->
  m (Response User.ListRsp)
usersList = liftNonClassy . flip NonClassy.usersList

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.lookupByEmail>
userLookupByEmail ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  User.Email ->
  m (Response User.UserRsp)
userLookupByEmail = liftNonClassy . flip NonClassy.userLookupByEmail

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
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  -- | The first request to send. _NOTE_: 'Conversation.historyReqCursor' is silently ignored.
  Conversation.HistoryReq ->
  -- | An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
  m (LoadPage m Common.Message)
conversationsHistoryAll = fetchAllBy conversationsHistory

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
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  -- | The first request to send. _NOTE_: 'Conversation.repliesReqCursor' is silently ignored.
  Conversation.RepliesReq ->
  -- | An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
  m (LoadPage m Common.Message)
repliesFetchAll = fetchAllBy conversationsReplies

liftNonClassy ::
  (MonadReader env m, HasManager env, HasToken env, MonadIO m) =>
  (SlackConfig -> IO a) ->
  m a
liftNonClassy f =
  liftToReader $ \env -> f $ SlackConfig (getManager env) (getToken env)

liftToReader ::
  (MonadReader env m, MonadIO m) =>
  (env -> IO a) ->
  m a
liftToReader f = do
  env <- ask
  liftIO $ f env
