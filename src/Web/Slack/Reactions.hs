{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | API methods relating to reactions:
--
-- - <https://api.slack.com/methods/reactions.add>
-- - <https://api.slack.com/methods/reactions.get>
-- - <https://api.slack.com/methods/reactions.list>
-- - <https://api.slack.com/methods/reactions.remove>
--
-- @since 2.1.0.0
module Web.Slack.Reactions (
  AddReq (..),
  AddResp (..),
  RemoveReq (..),
  RemoveResp (..),
  Api,
  reactionsAdd,
  reactionsAdd_,
  reactionsRemove,
  reactionsRemove_,
) where

import Data.Aeson qualified as A
import Servant.API (AuthProtect, FormUrlEncoded, JSON, Post, ReqBody, (:<|>) (..), (:>))
import Servant.Client (ClientM, client)
import Servant.Client.Core (AuthenticatedRequest)
import Web.FormUrlEncoded (ToForm (..))
import Web.Slack.Internal (ResponseJSON (..), SlackConfig (..), mkSlackAuthenticateReq, run)
import Web.Slack.Pager (Response)
import Web.Slack.Prelude
import Web.Slack.Types (ConversationId (..), Emoji (..))

-- | Add a reaction to a message.
--
-- <https://api.slack.com/methods/reactions.add>
--
-- @since 2.1.0.0
data AddReq = AddReq
  { channel :: ConversationId
  -- ^ Conversation in which to react.
  , name :: Emoji
  -- ^ Emoji name. For Unicode emoji, this is the name, not the character.
  , timestamp :: Text
  -- ^ Message @ts@ to react to.
  }
  deriving stock (Show, Eq)

instance ToForm AddReq where
  toForm AddReq {..} =
    [ ("channel", unConversationId channel)
    , ("name", unEmoji name)
    , ("timestamp", timestamp)
    ]

-- | Response to @reactions.add@. Slack doesn't send us anything here.
--
-- @since 2.1.0.0
data AddResp = AddResp
  deriving stock (Show, Eq)

instance ToJSON AddResp where
  toJSON _ = A.object []

instance FromJSON AddResp where
  parseJSON = A.withObject "Reactions.AddResp" \_ -> pure AddResp

-- | Remove a reaction from a message.
--
-- <https://api.slack.com/methods/reactions.remove>
--
-- @since 2.4.0.0
data RemoveReq = RemoveReq
  { removeReqChannel :: ConversationId
  -- ^ Conversation containing the message.
  , removeReqName :: Emoji
  -- ^ Emoji name. For Unicode emoji, this is the name, not the character.
  , removeReqTimestamp :: Text
  -- ^ Message @ts@ to remove the reaction from.
  }
  deriving stock (Show, Eq)

instance ToForm RemoveReq where
  toForm RemoveReq {..} =
    [ ("channel", unConversationId removeReqChannel)
    , ("name", unEmoji removeReqName)
    , ("timestamp", removeReqTimestamp)
    ]

-- | Response to @reactions.remove@. Slack doesn't send us anything here.
--
-- @since 2.4.0.0
data RemoveResp = RemoveResp
  deriving stock (Show, Eq)

instance ToJSON RemoveResp where
  toJSON _ = A.object []

instance FromJSON RemoveResp where
  parseJSON = A.withObject "Reactions.RemoveResp" \_ -> pure RemoveResp

type Api =
  "reactions.add"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] AddReq
    :> Post '[JSON] (ResponseJSON AddResp)
    :<|> "reactions.remove"
      :> AuthProtect "token"
      :> ReqBody '[FormUrlEncoded] RemoveReq
      :> Post '[JSON] (ResponseJSON RemoveResp)

-- | Adds a reaction to a message.
--
-- <https://api.slack.com/methods/reactions.add>
--
-- @since 2.1.0.0
reactionsAdd :: SlackConfig -> AddReq -> IO (Response AddResp)
reactionsAdd slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (reactionsAdd_ authR req) . slackConfigManager $ slackConfig

-- | Removes a reaction from a message.
--
-- <https://api.slack.com/methods/reactions.remove>
--
-- @since 2.4.0.0
reactionsRemove :: SlackConfig -> RemoveReq -> IO (Response RemoveResp)
reactionsRemove slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (reactionsRemove_ authR req) . slackConfigManager $ slackConfig

reactionsAdd_ :: AuthenticatedRequest (AuthProtect "token") -> AddReq -> ClientM (ResponseJSON AddResp)

-- | @since 2.4.0.0
reactionsRemove_ :: AuthenticatedRequest (AuthProtect "token") -> RemoveReq -> ClientM (ResponseJSON RemoveResp)
reactionsAdd_ :<|> reactionsRemove_ = client (Proxy @Api)

-- FIXME(jadel): reactions.get, reactions.list
