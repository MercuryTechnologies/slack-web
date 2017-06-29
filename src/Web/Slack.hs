{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack
  ( run
  , mkManager
  , apiTest
  , authTest
  , chatPostMessage
  , channelsCreate
  , channelsList
  , channelsHistory
  , groupsHistory
  , groupsList
  , historyFetchAll
  , imHistory
  , imList
  , mpimList
  , mpimHistory
  , usersList
  , authenticateReq
  )
  where

-- base
import Data.Maybe
import Data.Proxy (Proxy(..))

-- error
import Control.Error (lastZ, isNothing)

-- http-client
import Network.HTTP.Client (Manager, newManager)

-- http-client-tls
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- servant
import Servant.API

-- servant-client
import Servant.Client
import Servant.Common.Req (Req, appendToQueryString)

-- slack-web
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Auth as Auth
import qualified Web.Slack.Channel as Channel
import qualified Web.Slack.Chat as Chat
import qualified Web.Slack.Common as Common
import qualified Web.Slack.Im as Im
import qualified Web.Slack.Group as Group
import qualified Web.Slack.User as User

-- text
import Data.Text (Text)

-- transformers
import Control.Monad.IO.Class


-- |
--
--

type Api =
    "api.test"
      :> ReqBody '[FormUrlEncoded] Api.TestReq
      :> Post '[JSON] Api.TestRsp
  :<|>
    "auth.test"
      :> AuthProtect "token"
      :> Post '[JSON] Auth.TestRsp
  :<|>
    "channels.create"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.CreateReq
      :> Post '[JSON] Channel.CreateRsp
  :<|>
    "channels.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] Common.HistoryRsp
  :<|>
    "channels.list"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.ListReq
      :> Post '[JSON] Channel.ListRsp
  :<|>
    "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] Chat.PostMsgRsp
  :<|>
    "groups.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] Common.HistoryRsp
  :<|>
     "groups.list"
      :> AuthProtect "token"
      :> Post '[JSON] Group.ListRsp
  :<|>
    "im.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] Common.HistoryRsp
  :<|>
    "im.list"
      :> AuthProtect "token"
      :> Post '[JSON] Im.ListRsp
  :<|>
    "mpim.list"
      :> AuthProtect "token"
      :> Post '[JSON] Group.ListRsp
  :<|>
    "mpim.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] Common.HistoryRsp
  :<|>
    "users.list"
      :> AuthProtect "token"
      :> Post '[JSON] User.ListRsp


-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>

apiTest
  :: Api.TestReq
  -> ClientM Api.TestRsp


-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>

authTest
  :: Text
  -> ClientM Auth.TestRsp
authTest token =
  authTest_ (mkAuthenticateReq token authenticateReq)

authTest_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM Auth.TestRsp


-- |
--
-- Create a channel.
--
-- <https://api.slack.com/methods/channels.create>

channelsCreate
  :: Text
  -> Channel.CreateReq
  -> ClientM Channel.CreateRsp
channelsCreate token =
  channelsCreate_ (mkAuthenticateReq token authenticateReq)

channelsCreate_
  :: AuthenticateReq (AuthProtect "token")
  -> Channel.CreateReq
  -> ClientM Channel.CreateRsp

-- |
--
-- Retrieve channel list.
--
-- <https://api.slack.com/methods/channels.list>

channelsList
  :: Text
  -> Channel.ListReq
  -> ClientM Channel.ListRsp
channelsList token =
  channelsList_ (mkAuthenticateReq token authenticateReq)

channelsList_
  :: AuthenticateReq (AuthProtect "token")
  -> Channel.ListReq
  -> ClientM Channel.ListRsp

-- |
--
-- Retrieve channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/channels.history>

channelsHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp
channelsHistory token =
  channelsHistory_ (mkAuthenticateReq token authenticateReq)

channelsHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>

chatPostMessage
  :: Text
  -> Chat.PostMsgReq
  -> ClientM Chat.PostMsgRsp
chatPostMessage token =
  chatPostMessage_ (mkAuthenticateReq token authenticateReq)

chatPostMessage_
  :: AuthenticateReq (AuthProtect "token")
  -> Chat.PostMsgReq
  -> ClientM Chat.PostMsgRsp

-- |
--
-- This method returns a list of private channels in the team that the caller
-- is in and archived groups that the caller was in. The list of
-- (non-deactivated) members in each private channel is also returned.
--
-- <https://api.slack.com/methods/groups.list>

groupsList
  :: Text
  -> ClientM Group.ListRsp
groupsList token =
  groupsList_ (mkAuthenticateReq token authenticateReq)

groupsList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM Group.ListRsp

-- |
--
-- This method returns a portion of messages/events from the specified
-- private channel. To read the entire history for a private channel,
-- call the method with no latest or oldest arguments, and then continue paging.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/groups.history>

groupsHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp
groupsHistory token =
  groupsHistory_ (mkAuthenticateReq token authenticateReq)

groupsHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp

-- |
--
-- Returns a list of all direct message channels that the user has
--
-- <https://api.slack.com/methods/im.list>

imList
  :: Text
  -> ClientM Im.ListRsp
imList token =
  imList_ (mkAuthenticateReq token authenticateReq)

imList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM Im.ListRsp

-- |
--
-- Retrieve direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/im.history>

imHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp
imHistory token =
  imHistory_ (mkAuthenticateReq token authenticateReq)

imHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp

-- |
--
-- Returns a list of all multiparty direct message channels that the user has
--
-- <https://api.slack.com/methods/mpim.list>

mpimList
  :: Text
  -> ClientM Group.ListRsp
mpimList token =
  mpimList_ (mkAuthenticateReq token authenticateReq)

mpimList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM Group.ListRsp

-- |
--
-- Retrieve multiparty direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/mpim.history>

mpimHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp
mpimHistory token =
  mpimHistory_ (mkAuthenticateReq token authenticateReq)

mpimHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM Common.HistoryRsp

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>

usersList
  :: Text
  -> ClientM User.ListRsp
usersList token =
  usersList_ (mkAuthenticateReq token authenticateReq)

usersList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM User.ListRsp

-- |
-- Fetch all history items between two dates. The basic calls
-- 'channelsHistory', 'groupsHistory', 'imHistory' and so on
-- may not return exhaustive results if there were too many
-- records. You need to use 'historyRspHasMore' to find out
-- whether you got all the data.
--
-- This function will repeatedly call the underlying history
-- function until all the data is fetched or until a call
-- fails, merging the messages obtained from each call.
historyFetchAll
  :: Text -> (Text -> Common.HistoryReq -> ClientM Common.HistoryRsp)
  -> Text -> Int -> Common.SlackTimestamp -> Common.SlackTimestamp
  -> ClientM Common.HistoryRsp
historyFetchAll token makeReq channel count oldest latest = do
    rsp@(Common.HistoryRsp msgs hasMore) <-
        makeReq token (Common.HistoryReq channel count (Just latest) (Just oldest) False)
    -- From slack apidoc: If there are more than 100 messages between
    -- the two timestamps then the messages returned are the ones closest to latest.
    -- In most cases an application will want the most recent messages
    -- and will page backward from there.
    --
    -- for reference (does not apply here) => If oldest is provided but not
    -- latest then the messages returned are those closest to oldest,
    -- allowing you to page forward through history if desired.
    let oldestReceived = Common.messageTs <$> lastZ msgs
    if not hasMore || isNothing oldestReceived
        then return rsp
        else mergeResponses msgs <$>
             historyFetchAll token makeReq channel count oldest (fromJust oldestReceived)

mergeResponses
  :: [Common.Message]
  -> Common.HistoryRsp
  -> Common.HistoryRsp
mergeResponses msgs rsp = rsp { Common.historyRspMessages = msgs ++ Common.historyRspMessages rsp }

apiTest
  :<|> authTest_
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
  -> Req
  -> Req
authenticateReq token =
  appendToQueryString "token" (Just token)


-- |
--
--

run
  :: MonadIO m
  => Manager
  -> ClientM a
  -> m (Either ServantError a)
run manager =
  let
    baseUrl =
      BaseUrl Https "slack.com" 443 "/api"

  in
    liftIO . flip runClientM (ClientEnv manager baseUrl)


-- |
--
--

mkManager :: IO Manager
mkManager =
  newManager tlsManagerSettings
