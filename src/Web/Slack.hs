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
  , channelsCreate
  , conversationsList
  , conversationsHistory
  , channelsList
  , channelsHistory
  , groupsHistory
  , groupsList
  , historyFetchAll
  , imHistory
  , imList
  , mpimList
  , mpimHistory
  , getUserDesc
  , usersList
  , userLookupByEmail
  , authenticateReq
  , Response
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

-- error
import Control.Error (lastZ, isNothing)

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
import qualified Web.Slack.Channel as Channel
import qualified Web.Slack.Chat as Chat
import qualified Web.Slack.Common as Common
import qualified Web.Slack.Im as Im
import qualified Web.Slack.Group as Group
import qualified Web.Slack.User as User

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

type Response a =  Either Common.SlackClientError a

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
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] (ResponseJSON Conversation.HistoryRsp)
  :<|>
    "channels.create"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.CreateReq
      :> Post '[JSON] (ResponseJSON Channel.CreateRsp)
  :<|>
    "channels.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] (ResponseJSON Common.HistoryRsp)
  :<|>
    "channels.list"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.ListReq
      :> Post '[JSON] (ResponseJSON Channel.ListRsp)
  :<|>
    "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] (ResponseJSON Chat.PostMsgRsp)
  :<|>
    "groups.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] (ResponseJSON Common.HistoryRsp)
  :<|>
     "groups.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Group.ListRsp)
  :<|>
    "im.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] (ResponseJSON Common.HistoryRsp)
  :<|>
    "im.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Im.ListRsp)
  :<|>
    "mpim.list"
      :> AuthProtect "token"
      :> Post '[JSON] (ResponseJSON Group.ListRsp)
  :<|>
    "mpim.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] (ResponseJSON Common.HistoryRsp)
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
-- _NOTE_: the return type 'Conversation.HistoryRsp' is a different type
--         from 'Common.HistoryRsp' of 'Web.Slack.Common'.
--
-- <https://api.slack.com/methods/conversations.history>

conversationsHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Common.HistoryReq
  -> m (Response Conversation.HistoryRsp)
conversationsHistory histReq = do
  authR <- mkSlackAuthenticateReq
  run (conversationsHistory_ authR histReq)

conversationsHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Conversation.HistoryRsp)


-- |
--
-- Create a channel.
--
-- <https://api.slack.com/methods/channels.create>

channelsCreate
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Channel.CreateReq
  -> m (Response Channel.CreateRsp)
channelsCreate createReq = do
  authR <- mkSlackAuthenticateReq
  run (channelsCreate_ authR createReq)

channelsCreate_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Channel.CreateReq
  -> ClientM (ResponseJSON Channel.CreateRsp)

-- |
--
-- Retrieve channel list.
--
-- <https://api.slack.com/methods/channels.list>

channelsList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Channel.ListReq
  -> m (Response Channel.ListRsp)
channelsList listReq = do
  authR <- mkSlackAuthenticateReq
  run (channelsList_ authR listReq)

channelsList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Channel.ListReq
  -> ClientM (ResponseJSON Channel.ListRsp)

-- |
--
-- Retrieve channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/channels.history>

channelsHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Common.HistoryReq
  -> m (Response Common.HistoryRsp)
channelsHistory histReq = do
  authR <- mkSlackAuthenticateReq
  run (channelsHistory_ authR histReq)

channelsHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

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
-- This method returns a list of private channels in the team that the caller
-- is in and archived groups that the caller was in. The list of
-- (non-deactivated) members in each private channel is also returned.
--
-- <https://api.slack.com/methods/groups.list>

groupsList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response Group.ListRsp)
groupsList = do
  authR <- mkSlackAuthenticateReq
  run (groupsList_ authR)

groupsList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON Group.ListRsp)

-- |
--
-- This method returns a portion of messages/events from the specified
-- private channel. To read the entire history for a private channel,
-- call the method with no latest or oldest arguments, and then continue paging.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/groups.history>

groupsHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Common.HistoryReq
  -> m (Response Common.HistoryRsp)
groupsHistory hisReq = do
  authR <- mkSlackAuthenticateReq
  run (groupsHistory_ authR hisReq)

groupsHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- Returns a list of all direct message channels that the user has
--
-- <https://api.slack.com/methods/im.list>

imList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response Im.ListRsp)
imList = do
  authR <- mkSlackAuthenticateReq
  run (imList_ authR)

imList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON Im.ListRsp)

-- |
--
-- Retrieve direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/im.history>

imHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Common.HistoryReq
  -> m (Response Common.HistoryRsp)
imHistory histReq = do
  authR <- mkSlackAuthenticateReq
  run (imHistory_ authR histReq)

imHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- Returns a list of all multiparty direct message channels that the user has
--
-- <https://api.slack.com/methods/mpim.list>

mpimList
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => m (Response Group.ListRsp)
mpimList = do
  authR <- mkSlackAuthenticateReq
  run (mpimList_ authR)

mpimList_
  :: AuthenticatedRequest (AuthProtect "token")
  -> ClientM (ResponseJSON Group.ListRsp)

-- |
--
-- Retrieve multiparty direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/mpim.history>

mpimHistory
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => Common.HistoryReq
  -> m (Response Common.HistoryRsp)
mpimHistory histReq = do
  authR <- mkSlackAuthenticateReq
  run (mpimHistory_ authR histReq)

mpimHistory_
  :: AuthenticatedRequest (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

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

-- |
-- Fetch all history items between two dates. The basic calls
-- 'channelsHistory', 'groupsHistory', 'imHistory' and so on
-- may not return exhaustive results if there were too many
-- records. You need to use 'Web.Slack.Common.historyRspHasMore' to find out
-- whether you got all the data.
--
-- This function will repeatedly call the underlying history
-- function until all the data is fetched or until a call
-- fails, merging the messages obtained from each call.
historyFetchAll
  :: (MonadReader env m, HasManager env, HasToken env, MonadIO m)
  => (Common.HistoryReq -> m (Response Common.HistoryRsp))
  -- ^ The request to make. Can be for instance 'mpimHistory', 'channelsHistory'...
  -> Text
  -- ^ The channel name to query
  -> Int
  -- ^ The number of entries to fetch at once.
  -> Common.SlackTimestamp
  -- ^ The oldest timestamp to fetch records from
  -> Common.SlackTimestamp
  -- ^ The newest timestamp to fetch records to
  -> m (Response Common.HistoryRsp)
  -- ^ A list merging all the history records that were fetched
  -- through the individual queries.
historyFetchAll makeReq channel count oldest latest = do
    -- From slack apidoc: If there are more than 100 messages between
    -- the two timestamps then the messages returned are the ones closest to latest.
    -- In most cases an application will want the most recent messages
    -- and will page backward from there.
    --
    -- for reference (does not apply here) => If oldest is provided but not
    -- latest then the messages returned are those closest to oldest,
    -- allowing you to page forward through history if desired.
    rsp <- makeReq $ Common.HistoryReq channel count (Just latest) (Just oldest) False
    case rsp of
      Left _ -> return rsp
      Right (Common.HistoryRsp msgs hasMore) -> do
          let oldestReceived = Common.messageTs <$> lastZ msgs
          if not hasMore || isNothing oldestReceived
              then return rsp
              else mergeResponses msgs <$>
                   historyFetchAll makeReq channel count oldest (fromJust oldestReceived)

mergeResponses
  :: [Common.Message]
  -> Response Common.HistoryRsp
  -> Response Common.HistoryRsp
mergeResponses _ err@(Left _) = err
mergeResponses msgs (Right rsp) =
    Right (rsp { Common.historyRspMessages = msgs ++ Common.historyRspMessages rsp })

apiTest_
  :<|> authTest_
  :<|> conversationsList_
  :<|> conversationsHistory_
  :<|> channelsCreate_
  :<|> channelsHistory_
  :<|> channelsList_
  :<|> chatPostMessage_
  :<|> groupsHistory_
  :<|> groupsList_
  :<|> imHistory_
  :<|> imList_
  :<|> mpimList_
  :<|> mpimHistory_
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
