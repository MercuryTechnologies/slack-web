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
module Web.Slack.Reactions (AddReq (..), AddResp (..), Api, reactionsAdd, reactionsAdd_) where

import Data.Aeson qualified as A
import Servant.API (AuthProtect, FormUrlEncoded, JSON, Post, ReqBody, (:>))
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

type Api =
  "reactions.add"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] AddReq
    :> Post '[JSON] (ResponseJSON AddResp)

-- | Adds a reaction to a message.
--
-- <https://api.slack.com/methods/reactions.add>
--
-- @since 2.1.0.0
reactionsAdd :: SlackConfig -> AddReq -> IO (Response AddResp)
reactionsAdd slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (reactionsAdd_ authR req) . slackConfigManager $ slackConfig

reactionsAdd_ :: AuthenticatedRequest (AuthProtect "token") -> AddReq -> ClientM (ResponseJSON AddResp)
reactionsAdd_ = client (Proxy @Api)

-- FIXME(jadel): reactions.remove, reactions.get, reactions.list
