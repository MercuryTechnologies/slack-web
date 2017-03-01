{-# LANGUAGE DataKinds #-}
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

-- slack-web
import qualified Web.Slack.Api as Api
import qualified Web.Slack.Auth as Auth
import qualified Web.Slack.Channel as Channel
import qualified Web.Slack.Chat as Chat

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
      :> ReqBody '[FormUrlEncoded] Auth.TestReq
      :> Post '[JSON] Auth.TestRsp
  :<|>
    "channels.create"
      :> ReqBody '[FormUrlEncoded] Channel.CreateReq
      :> Post '[JSON] Channel.CreateRsp
  :<|>
    "chat.postMessage"
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
  :: Auth.TestReq
  -> ClientM Auth.TestRsp

-- |
--
-- Create a channel.
--
-- <https://api.slack.com/methods/channels.create>

channelsCreate
  :: Channel.CreateReq
  -> ClientM Channel.CreateRsp

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>

chatPostMessage
  :: Chat.PostMsgReq
  -> ClientM Chat.PostMsgRsp

apiTest
  :<|> authTest
  :<|> channelsCreate
  :<|> chatPostMessage
  =
  client (Proxy :: Proxy Api)


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
