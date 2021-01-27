{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack
  ( SlackConfig(..)
  , mkSlackConfig
  , apiTest
  , authTest
  , chatPostMessage
  , conversationsList
  , conversationsHistory
  , conversationsHistoryAll
  , conversationsReplies
  , repliesFetchAll
  , getUserDesc
  , usersList
  , userLookupByEmail
  , authenticateReq
  , Response
  , LoadPage
  , HasManager(..)
  , HasToken(..)
  )
  where

-- aeson
import Data.Aeson

-- base
import Control.Arrow ((&&&))
import Data.Maybe
import Data.Proxy (Proxy(..))

-- containers
import qualified Data.Map as Map

-- http-client
import Network.HTTP.Client (Manager, newManager)

-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- mtl
import Control.Monad.Reader

-- servant
import Servant.API

-- servant-client
import Servant.Client hiding (Response, baseUrl)
import Servant.Client.Core (Request, appendToQueryString)

-- slack-web
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Auth as Auth
import qualified Web.Slack.Conversation as Conversation
import qualified Web.Slack.Chat as Chat
import qualified Web.Slack.Common as Common
import qualified Web.Slack.User as User
import           Web.Slack.Pager

-- text
import Data.Text (Text)

#if !MIN_VERSION_servant(0,13,0)
mkClientEnv :: Manager -> BaseUrl -> ClientEnv
mkClientEnv = ClientEnv
#endif

#if MIN_VERSION_servant(0,16,0)
import Servant.Client.Core (AuthenticatedRequest, AuthClientData, mkAuthenticatedRequest, ClientError)
#else
import Servant.Client.Core.Internal.Auth
import Servant.Client.Core (ServantError)
type ClientError = ServantError
#endif

class HasManager a where
    getManager :: a -> Manager

class HasToken a where
    getToken :: a -> Text

-- | Implements the 'HasManager' and 'HasToken' typeclasses.
data SlackConfig
  = SlackConfig
  { slackConfigManager :: Manager
  , slackConfigToken :: Text
  }

instance HasManager SlackConfig where
    getManager = slackConfigManager
instance HasToken SlackConfig where
    getToken = slackConfigToken

-- contains errors that can be returned by the slack API.
-- constrast with 'SlackClientError' which additionally
-- contains errors which occured during the network communication.
data ResponseSlackError = ResponseSlackError Text
  deriving (Eq, Show)


-- |
-- Internal type!
--
newtype ResponseJSON a = ResponseJSON (Either ResponseSlackError a)

instance FromJSON a => FromJSON (ResponseJSON a) where
    parseJSON = withObject "Response" $ \o -> do
        ok <- o .: "ok"
        ResponseJSON <$> if ok
           then Right <$> parseJSON (Object o)
           else Left . ResponseSlackError <$> o .: "error"


-- |
--
--

type Api =
    "api.test"
      :> ReqBody '[FormUrlEncoded] Api.TestReq
      :> Post '[JSON] (ResponseJSON Api.TestRsp)
  :<|>
    "auth.test"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Auth.TestRsp)
  :<|>
    "conversations.list"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.ListReq
      :> Post '[JSON] (ResponseJSON Conversation.ListRsp)
  :<|>
    "conversations.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.HistoryReq
      :> Post '[JSON] (ResponseJSON Conversation.HistoryRsp)
  :<|>
    "conversations.replies"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Conversation.RepliesReq
      :> Post '[JSON] (ResponseJSON Conversation.HistoryRsp)
  :<|>
    "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] (ResponseJSON Chat.PostMsgRsp)
  :<|>
    "users.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON User.ListRsp)
  :<|>
    "users.lookupByEmail"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] User.Email
      :> Post '[JSON] (ResponseJSON User.UserRsp)


-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>

apiTest
  :: (MonadReader env m, HasManager env, MonadIO m)
  => Api.TestReq
  -> m (Response Api.TestRsp)
apiTest req = run (apiTest_ req)

apiTest_
  :: Api.TestReq
  -> ClientM (ResponseJSON Api.TestRsp)

-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>

authTest
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response Auth.TestRsp)
authTest = do
  authR <- mkSlackAuthenticateReq
  run (authTest_ authR)

authTest_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON Auth.TestRsp)

-- |
--
-- Retrieve conversations list.
--
-- <https://api.slack.com/methods/conversations.list>

conversationsList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Conversation.ListReq
  -> m (Response Conversation.ListRsp)
conversationsList listReq = do
  authR <- mkSlackAuthenticateReq
  run (conversationsList_ authR listReq)

conversationsList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Conversation.ListReq
  -> ClientM (ResponseJSON Conversation.ListRsp)


-- |
--
-- Retrieve ceonversation history.
-- Consider using 'historyFetchAll' in combination with this function.
--
-- <https://api.slack.com/methods/conversations.history>

conversationsHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Conversation.HistoryReq
  -> m (Response Conversation.HistoryRsp)
conversationsHistory histReq = do
  authR <- mkSlackAuthenticateReq
  run (conversationsHistory_ authR histReq)

conversationsHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Conversation.HistoryReq
  -> ClientM (ResponseJSON Conversation.HistoryRsp)


-- |
--
-- Retrieve replies of a conversation.
-- Consider using 'repliesFetchAll' if you want to get entire replies
-- of a conversation.
--
-- <https://api.slack.com/methods/conversations.replies>

conversationsReplies
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Conversation.RepliesReq
  -> m (Response Conversation.HistoryRsp)
conversationsReplies repliesReq = do
  authR <- mkSlackAuthenticateReq
  run (conversationsReplies_ authR repliesReq)

conversationsReplies_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Conversation.RepliesReq
  -> ClientM (ResponseJSON Conversation.HistoryRsp)


-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>

chatPostMessage
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Chat.PostMsgReq
  -> m (Response Chat.PostMsgRsp)
chatPostMessage postReq = do
  authR <- mkSlackAuthenticateReq
  run (chatPostMessage_ authR postReq)

chatPostMessage_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Chat.PostMsgReq
  -> ClientM (ResponseJSON Chat.PostMsgRsp)


-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>

usersList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response User.ListRsp)
usersList = do
  authR <- mkSlackAuthenticateReq
  run (usersList_ authR)

usersList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON User.ListRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.lookupByEmail>

userLookupByEmail
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => User.Email 
  -> m (Response User.UserRsp)
userLookupByEmail email = do
  authR <- mkSlackAuthenticateReq
  run (userLookupByEmail_ authR email)

userLookupByEmail_
  :: AuthenticatedRequest (AuthProtect "token")
  -> User.Email
  -> ClientM (ResponseJSON User.UserRsp)


-- | Returns a function to get a username from a 'Common.UserId'.
-- Comes in handy to use 'Web.Slack.MessageParser.messageToHtml'
getUserDesc
  :: (Common.UserId -> Text)
  -- ^ A function to give a default username in case the username is unknown
  -> User.ListRsp
  -- ^ List of users as known by the slack server. See 'usersList'.
  -> (Common.UserId -> Text)
  -- ^ A function from 'Common.UserId' to username.
getUserDesc unknownUserFn users =
  let userMap = Map.fromList $ (User.userId &&& User.userName) <$> User.listRspMembers users
  in
    \userId -> fromMaybe (unknownUserFn userId) $ Map.lookup userId userMap


-- | Returns an action to send a request to get the history of a conversation.
--
--   To fetch all messages in the conversation, run the returned 'LoadPage' action
--   repeatedly until it returns an empty list.
conversationsHistoryAll
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  =>  Conversation.HistoryReq
  -- ^ The first request to send. _NOTE_: 'Conversation.historyReqCursor' is silently ignored.
  -> m (LoadPage m Common.Message)
  -- ^ An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
conversationsHistoryAll = conversationsHistoryAllBy conversationsHistory


-- | Returns an action to send a request to get the replies of a conversation.
--
--   To fetch all replies in the conversation, run the returned 'LoadPage' action
--   repeatedly until it returns an empty list.
--
--   *NOTE*: The conversations.replies endpoint always returns the first message
--           of the thread. So every page returned by the 'LoadPage' action includes
--           the first message of the thread. You should drop it if you want to
--           collect messages in a thread without duplicates.
repliesFetchAll
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  =>  Conversation.RepliesReq
  -- ^ The first request to send. _NOTE_: 'Conversation.repliesReqCursor' is silently ignored.
  -> m (LoadPage m Common.Message)
  -- ^ An action which returns a new page of messages every time called.
  --   If there are no pages anymore, it returns an empty list.
repliesFetchAll = repliesFetchAllBy conversationsReplies

apiTest_
  :<|> authTest_
  :<|> conversationsList_
  :<|> conversationsHistory_
  :<|> conversationsReplies_
  :<|> chatPostMessage_
  :<|> usersList_
  :<|> userLookupByEmail_
  =
  client (Proxy :: Proxy Api)


-- |
--
--

type instance AuthClientData (AuthProtect "token") =
  Text


-- |
--
--

authenticateReq
  :: Text
  -> Request
  -> Request
authenticateReq token =
  appendToQueryString "token" (Just token)


-- |
--
--

run
  :: (MonadReader env m, HasManager env, MonadIO m)
  => ClientM (ResponseJSON a)
  -> m (Response a)
run clientAction = do
  env <- ask
  let baseUrl = BaseUrl Https "slack.com" 443 "/api"
  unnestErrors <$> liftIO (runClientM clientAction $ mkClientEnv (getManager env) baseUrl)

mkSlackAuthenticateReq :: (MonadReader env m, HasToken env)
  => m (AuthenticatedRequest (AuthProtect "token"))
mkSlackAuthenticateReq = flip mkAuthenticatedRequest authenticateReq . getToken <$> ask

unnestErrors :: Either ClientError (ResponseJSON a) -> Response a
unnestErrors (Right (ResponseJSON (Right a))) = Right a
unnestErrors (Right (ResponseJSON (Left (ResponseSlackError serv))))
    = Left (Common.SlackError serv)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)


-- | Prepare a SlackConfig from a slack token.
-- You can then call the other functions providing this in a reader context.
--
mkSlackConfig :: Text -> IO SlackConfig
mkSlackConfig token = SlackConfig <$> newManager tlsManagerSettings <*> pure token
