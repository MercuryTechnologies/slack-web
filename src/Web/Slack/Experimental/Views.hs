{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Publishing views using the Blocks API.
--
-- This is used for App Home and modals.
--
-- <https://api.slack.com/methods/views.publish>
--
-- @since 2.1.0.0
module Web.Slack.Experimental.Views (
  -- * Types
  SlackView (..),
  HomeTabView (..),
  ModalView (..),
  Expected (..),

  -- * Requests and responses
  PublishReq (..),
  PublishResp (..),
  Api,
  viewsPublish,
) where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Servant.API (AuthProtect, FormUrlEncoded, JSON, Post, ReqBody, (:>))
import Servant.Client (ClientM, client)
import Servant.Client.Core (AuthenticatedRequest)
import Web.FormUrlEncoded (ToForm (..))
import Web.HttpApiData (ToHttpApiData (..))
import Web.Slack.AesonUtils (Expected (..), snakeCaseOptions, snakeCaseOptionsEatTrailingUnderscore, (.=!), (.=?))
import Web.Slack.Experimental.Blocks qualified as B
import Web.Slack.Experimental.Blocks.Types (SlackPlainTextOnly)
import Web.Slack.Internal (ResponseJSON (..), SlackConfig (..), mkSlackAuthenticateReq, run)
import Web.Slack.Pager (Response)
import Web.Slack.Prelude
import Web.Slack.Types (UserId)

-- | View definition for some Slack surface. Has an inner type of either
-- 'ModalView' or 'HomeTabView'.
--
-- <https://api.slack.com/reference/surfaces/views>
--
-- @since 2.1.0.0
data SlackView inner = SlackView
  { blocks :: Vector B.SlackBlock
  , privateMetadata :: Maybe Text
  , callbackId :: Maybe Text
  , externalId :: Maybe Text
  , inner :: inner
  }
  deriving stock (Show, Eq, Generic)

type role SlackView representational

instance (FromJSON inner) => FromJSON (SlackView inner) where
  parseJSON = withObject "SlackView" \o -> do
    blocks <- o .: "blocks"
    privateMetadata <- o .:? "private_metadata"
    callbackId <- o .:? "callback_id"
    externalId <- o .:? "external_id"
    inner <- parseJSON @inner (A.Object o)
    pure SlackView {..}

instance (ToJSON inner) => ToJSON (SlackView inner) where
  toJSON SlackView {..} = case toJSON inner of
    A.Object innerObj ->
      A.Object
        ( KM.fromList
            ( catMaybes
                [ "blocks" .=! blocks
                , "private_metadata" .=? privateMetadata
                , "callback_id" .=? callbackId
                , "external_id" .=? externalId
                ]
            )
            <> innerObj
        )
    _ -> error "inner of SlackView is not an object"

-- | @since 2.1.0.0
data HomeTabView = HomeTabView
  { type_ :: Expected "home"
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON snakeCaseOptionsEatTrailingUnderscore ''HomeTabView)

-- | Modal view
--
-- <https://api.slack.com/reference/surfaces/views#modal>
--
-- @since 2.1.0.0
data ModalView = ModalView
  { type_ :: Expected "modal"
  , title :: SlackPlainTextOnly
  -- ^ Title of the modal on the top left. Maximum length of 24 characters.
  , close :: Maybe SlackPlainTextOnly
  -- ^ Text appearing on the close button on the bottom-right of the modal.
  -- Maximum length of 24 characters.
  , submit :: Maybe SlackPlainTextOnly
  -- ^ Text appearing on the submit buttonn. Maximum length of 24 characters.
  -- Must be 'SlackPlainTextOnly'.
  , submitDisabled :: Maybe Bool
  -- ^ Whether one or more inputs must be filled before enabling the submit button.
  -- For configuration modals: <https://api.slack.com/reference/workflows/configuration-view>
  }
  deriving stock (Show, Eq, Generic)

$(deriveJSON snakeCaseOptionsEatTrailingUnderscore ''ModalView)

-- | Publishes the App Home view for a user.
--
-- <https://api.slack.com/methods/views.publish>
--
-- @since 2.1.0.0
data PublishReq = PublishReq
  { userId :: UserId
  -- ^ User to whom the view is being published.
  , view :: SlackView HomeTabView
  -- ^ View payload.
  }
  deriving stock (Show)

instance ToForm PublishReq where
  toForm PublishReq {..} =
    [ ("user_id" :: Text, toQueryParam userId)
    , ("view", toQueryParam . decodeUtf8 $ A.encode view)
    ]

-- | @since 2.1.0.0
data PublishResp = PublishResp
  { view :: SlackView HomeTabView
  }
  deriving stock (Show)

$(deriveJSON snakeCaseOptions ''PublishResp)

-- | @since 2.1.0.0
type Api =
  "views.publish"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] PublishReq
    :> Post '[JSON] (ResponseJSON PublishResp)

-- | Publishes the App Home view for a user.
--
-- <https://api.slack.com/methods/views.publish>
--
-- @since 2.1.0.0
viewsPublish :: SlackConfig -> PublishReq -> IO (Response PublishResp)
viewsPublish slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (viewsPublish_ authR req) . slackConfigManager $ slackConfig

viewsPublish_ :: AuthenticatedRequest (AuthProtect "token") -> PublishReq -> ClientM (ResponseJSON PublishResp)
viewsPublish_ = client (Proxy @Api)
