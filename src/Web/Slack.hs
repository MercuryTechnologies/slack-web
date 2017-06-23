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
  , imHistory
  , imList
  , mpimList
  , mpimHistory
  , authenticateReq
  )
  where

-- base
import Data.Proxy (Proxy(..))

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
import qualified Web.Slack.Mpim as Mpim

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
      :> Post '[JSON] Mpim.ListRsp
  :<|>
    "mpim.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Common.HistoryReq
      :> Post '[JSON] Common.HistoryRsp


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
  -> ClientM Mpim.ListRsp
mpimList token =
  mpimList_ (mkAuthenticateReq token authenticateReq)

mpimList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM Mpim.ListRsp

-- |
--
-- Retrieve multiparty direct message channel history.
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

apiTest
  :<|> authTest_
  :<|> channelsCreate_
  :<|> channelsHistory_
  :<|> channelsList_
  :<|> chatPostMessage_
  :<|> imHistory_
  :<|> imList_
  :<|> mpimList_
  :<|> mpimHistory_
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
