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
  , Response -- blackbox for users of the library
  )
  where

-- aeson
import Data.Aeson

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

-- contains errors that can be returned by the slack API.
-- constrast with 'SlackClientError' which additionally
-- contains errors which occured during the network communication.
data ResponseSlackError = ResponseSlackError Text
  deriving (Eq, Show)

-- |
--
-- type is opaque to library users (that's why a newtype won't do)
newtype Response a = Response { unResponse :: Either ResponseSlackError a }

-- |
-- Internal type!
--
newtype ResponseJSON a = ResponseJSON { unResponseJSON :: Response a }

instance FromJSON a => FromJSON (ResponseJSON a) where
    parseJSON = withObject "Response" $ \o -> do
        ok <- o .: "ok"
        ResponseJSON . Response <$> if ok
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


-- |
--
-- Check API calling code.
--
-- <https://api.slack.com/methods/api.test>

apiTest
  :: Api.TestReq
  -> ClientM (Response Api.TestRsp)
apiTest req = unResponseJSON <$> apiTest_ req

apiTest_
  :: Api.TestReq
  -> ClientM (ResponseJSON Api.TestRsp)

-- |
--
-- Check authentication and identity.
--
-- <https://api.slack.com/methods/auth.test>

authTest
  :: Text
  -> ClientM (Response Auth.TestRsp)
authTest token = unResponseJSON <$>
  authTest_ (mkAuthenticateReq token authenticateReq)

authTest_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM (ResponseJSON Auth.TestRsp)


-- |
--
-- Create a channel.
--
-- <https://api.slack.com/methods/channels.create>

channelsCreate
  :: Text
  -> Channel.CreateReq
  -> ClientM (Response Channel.CreateRsp)
channelsCreate token = fmap unResponseJSON .
  channelsCreate_ (mkAuthenticateReq token authenticateReq)

channelsCreate_
  :: AuthenticateReq (AuthProtect "token")
  -> Channel.CreateReq
  -> ClientM (ResponseJSON Channel.CreateRsp)

-- |
--
-- Retrieve channel list.
--
-- <https://api.slack.com/methods/channels.list>

channelsList
  :: Text
  -> Channel.ListReq
  -> ClientM (Response Channel.ListRsp)
channelsList token = fmap unResponseJSON .
  channelsList_ (mkAuthenticateReq token authenticateReq)

channelsList_
  :: AuthenticateReq (AuthProtect "token")
  -> Channel.ListReq
  -> ClientM (ResponseJSON Channel.ListRsp)

-- |
--
-- Retrieve channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/channels.history>

channelsHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM (Response Common.HistoryRsp)
channelsHistory token = fmap unResponseJSON .
  channelsHistory_ (mkAuthenticateReq token authenticateReq)

channelsHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- Send a message to a channel.
--
-- <https://api.slack.com/methods/chat.postMessage>

chatPostMessage
  :: Text
  -> Chat.PostMsgReq
  -> ClientM (Response Chat.PostMsgRsp)
chatPostMessage token = fmap unResponseJSON .
  chatPostMessage_ (mkAuthenticateReq token authenticateReq)

chatPostMessage_
  :: AuthenticateReq (AuthProtect "token")
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
  :: Text
  -> ClientM (Response Group.ListRsp)
groupsList token = unResponseJSON <$>
  groupsList_ (mkAuthenticateReq token authenticateReq)

groupsList_
  :: AuthenticateReq (AuthProtect "token")
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
  :: Text
  -> Common.HistoryReq
  -> ClientM (Response Common.HistoryRsp)
groupsHistory token = fmap unResponseJSON .
  groupsHistory_ (mkAuthenticateReq token authenticateReq)

groupsHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- Returns a list of all direct message channels that the user has
--
-- <https://api.slack.com/methods/im.list>

imList
  :: Text
  -> ClientM (Response Im.ListRsp)
imList token = unResponseJSON <$>
  imList_ (mkAuthenticateReq token authenticateReq)

imList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM (ResponseJSON Im.ListRsp)

-- |
--
-- Retrieve direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/im.history>

imHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM (Response Common.HistoryRsp)
imHistory token = fmap unResponseJSON .
  imHistory_ (mkAuthenticateReq token authenticateReq)

imHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- Returns a list of all multiparty direct message channels that the user has
--
-- <https://api.slack.com/methods/mpim.list>

mpimList
  :: Text
  -> ClientM (Response Group.ListRsp)
mpimList token = unResponseJSON <$>
  mpimList_ (mkAuthenticateReq token authenticateReq)

mpimList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM (ResponseJSON Group.ListRsp)

-- |
--
-- Retrieve multiparty direct message channel history.
-- Consider using 'historyFetchAll' in combination with this function
--
-- <https://api.slack.com/methods/mpim.history>

mpimHistory
  :: Text
  -> Common.HistoryReq
  -> ClientM (Response Common.HistoryRsp)
mpimHistory token = fmap unResponseJSON .
  mpimHistory_ (mkAuthenticateReq token authenticateReq)

mpimHistory_
  :: AuthenticateReq (AuthProtect "token")
  -> Common.HistoryReq
  -> ClientM (ResponseJSON Common.HistoryRsp)

-- |
--
-- This method returns a list of all users in the team.
-- This includes deleted/deactivated users.
--
-- <https://api.slack.com/methods/users.list>

usersList
  :: Text
  -> ClientM (Response User.ListRsp)
usersList token = unResponseJSON <$>
  usersList_ (mkAuthenticateReq token authenticateReq)

usersList_
  :: AuthenticateReq (AuthProtect "token")
  -> ClientM (ResponseJSON User.ListRsp)

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
  :: Text -> (Text -> Common.HistoryReq -> ClientM (Response Common.HistoryRsp))
  -> Text -> Int -> Common.SlackTimestamp -> Common.SlackTimestamp
  -> ClientM (Response Common.HistoryRsp)
historyFetchAll token makeReq channel count oldest latest = do
    -- From slack apidoc: If there are more than 100 messages between
    -- the two timestamps then the messages returned are the ones closest to latest.
    -- In most cases an application will want the most recent messages
    -- and will page backward from there.
    --
    -- for reference (does not apply here) => If oldest is provided but not
    -- latest then the messages returned are those closest to oldest,
    -- allowing you to page forward through history if desired.
    rsp <- unResponse <$> makeReq token (Common.HistoryReq channel count (Just latest) (Just oldest) False)
    Response <$> case rsp of
      Left _ -> return rsp
      Right (Common.HistoryRsp msgs hasMore) -> do
          let oldestReceived = Common.messageTs <$> lastZ msgs
          if not hasMore || isNothing oldestReceived
              then return rsp
              else mergeResponses msgs <$>
                   historyFetchAll token makeReq channel count oldest (fromJust oldestReceived)

mergeResponses
  :: [Common.Message]
  -> Response Common.HistoryRsp
  -> Either ResponseSlackError Common.HistoryRsp
mergeResponses _ err@(Response (Left _)) = unResponse err
mergeResponses msgs (Response (Right rsp)) =
    Right (rsp { Common.historyRspMessages = msgs ++ Common.historyRspMessages rsp })

apiTest_
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
  -> ClientM (Response a)
  -> m (Either Common.SlackClientError a)
run manager =
  let
    baseUrl =
      BaseUrl Https "slack.com" 443 "/api"

  in
    fmap unnestErrors . liftIO . flip runClientM (ClientEnv manager baseUrl)

unnestErrors :: Either ServantError (Response a) -> Either Common.SlackClientError a
unnestErrors (Right (Response (Right a))) = Right a
unnestErrors (Right (Response (Left (ResponseSlackError serv)))) = Left (Common.SlackError serv)
unnestErrors (Left slackErr) = Left (Common.ServantError slackErr)


-- |
--
--

mkManager :: IO Manager
mkManager =
  newManager tlsManagerSettings
