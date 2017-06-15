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
    "channels.list"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.ListReq
      :> Post '[JSON] Channel.ListRsp
  :<|>
    "channels.history"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Channel.HistoryReq
      :> Post '[JSON] Channel.HistoryRsp
  :<|>
    "chat.postMessage"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] Chat.PostMsgReq
      :> Post '[JSON] Chat.PostMsgRsp


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
  -> Channel.HistoryReq
  -> ClientM Channel.HistoryRsp
channelsHistory token =
  channelsHistory_ (mkAuthenticateReq token authenticateReq)

channelsHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Channel.HistoryReq
  -> ClientM Channel.HistoryRsp

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


apiTest
  :<|> authTest_
  :<|> channelsCreate_
  :<|> channelsList_
  :<|> channelsHistory_
  :<|> chatPostMessage_
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
