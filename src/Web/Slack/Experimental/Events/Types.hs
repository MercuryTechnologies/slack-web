{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- FIXME(jadel): Use NoFieldSelectors when we drop everything before 9.2.

-- | Types for the [Slack Events API](https://api.slack.com/events).
module Web.Slack.Experimental.Events.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Time.Clock.System (SystemTime)
import Web.Slack.AesonUtils
import Web.Slack.Experimental.Blocks (SlackBlock)
import Web.Slack.Prelude
import Web.Slack.Types (ConversationId, TeamId, UserId)

-- | Not a ConversationType for some mysterious reason; this one has Channel as
-- an option, which does not exist as a ConversationType. Slack, why?
data ChannelType = Channel | Group | Im
  deriving stock (Show, Eq)

$(deriveJSON snakeCaseOptions ''ChannelType)

-- | <https://api.slack.com/events/message>
data MessageEvent = MessageEvent
  { blocks :: [SlackBlock]
  , channel :: ConversationId
  , text :: Text
  , channelType :: ChannelType
  , -- FIXME(jadel): clientMsgId??
    user :: UserId
  , ts :: Text
  , threadTs :: Maybe Text
  -- ^ Present if the message is in a thread
  , appId :: Maybe Text
  -- ^ Present if it's sent by an app
  --
  --   For mysterious reasons, this is NOT
  --   <https://api.slack.com/events/message/bot_message>, this is a regular
  --   message but with bot metadata. I Think it's because there *is* an
  --   associated user.
  --
  --   See @botMessage.json@ golden parser test.
  , botId :: Maybe Text
  -- ^ Present if it's sent by a bot user
  }
  deriving stock (Show)

-- | <https://api.slack.com/events/message/message_changed>
--
-- FIXME(jadel): implement. This is mega cursed! in the normal message event
-- the channel is called "channel" but here it is called "channelId" and also
-- has a "channel_name" and "channel_team". Why?!
--
-- We don't decode these on this basis.
data MessageUpdateEvent = MessageUpdateEvent
  { message :: MessageEvent
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''MessageEvent)
$(deriveFromJSON snakeCaseOptions ''MessageUpdateEvent)

-- | FIXME: this might be a Channel, but may also be missing some fields/have bonus
-- because Slack is cursed.
data CreatedChannel = CreatedChannel
  { id :: ConversationId
  , isChannel :: Bool
  , name :: Text
  , nameNormalized :: Text
  , creator :: UserId
  , created :: SystemTime
  , isShared :: Bool
  , isOrgShared :: Bool
  , -- what is this?
    contextTeamId :: TeamId
  }
  deriving stock (Show)

-- | A channel was created.
--
-- <https://api.slack.com/events/channel_created>
data ChannelCreatedEvent = ChannelCreatedEvent
  { channel :: CreatedChannel
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''CreatedChannel)
$(deriveFromJSON snakeCaseOptions ''ChannelCreatedEvent)

-- | You left a channel.
--
-- <https://api.slack.com/events/channel_left>
data ChannelLeftEvent = ChannelLeftEvent
  { actorId :: UserId
  , channel :: ConversationId
  , eventTs :: Text
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''ChannelLeftEvent)

-- | <https://api.slack.com/events/url_verification>
data UrlVerificationPayload = UrlVerificationPayload
  { challenge :: Text
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''UrlVerificationPayload)

newtype EventId = EventId {unEventId :: Text}
  deriving newtype (FromJSON)
  deriving stock (Show)

newtype MessageId = MessageId {unMessageId :: Text}
  deriving newtype (FromJSON)
  deriving stock (Show, Eq)

data Event
  = EventMessage MessageEvent
  | EventMessageChanged
  | -- | Weird message event of subtype channel_join. Sent "sometimes", according
    -- to a random Slack blog post from 2017:
    -- <https://api.slack.com/changelog/2017-05-rethinking-channel-entrance-and-exit-events-and-messages>
    --
    -- Documentation: <https://api.slack.com/events/message/channel_join>
    EventChannelJoinMessage
  | EventChannelCreated ChannelCreatedEvent
  | EventChannelLeft ChannelLeftEvent
  | EventUnknown Value
  deriving stock (Show)

instance FromJSON Event where
  parseJSON = withObject "MessageEvent" \obj -> do
    tag :: Text <- obj .: "type"
    subtype :: Maybe Text <- obj .:? "subtype"
    case (tag, subtype) of
      ("message", Nothing) -> EventMessage <$> parseJSON @MessageEvent (Object obj)
      ("message", Just "message_changed") -> pure EventMessageChanged
      ("message", Just "channel_join") -> pure EventChannelJoinMessage
      ("channel_created", Nothing) -> EventChannelCreated <$> parseJSON (Object obj)
      ("channel_left", Nothing) -> EventChannelLeft <$> parseJSON (Object obj)
      _ -> pure $ EventUnknown (Object obj)

data EventCallback = EventCallback
  { eventId :: EventId
  , teamId :: TeamId
  , eventTime :: SystemTime
  , event :: Event
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''EventCallback)

data SlackWebhookEvent
  = EventUrlVerification UrlVerificationPayload
  | EventEventCallback EventCallback
  | EventUnknownWebhook Value
  deriving stock (Show)

instance FromJSON SlackWebhookEvent where
  parseJSON = withObject "SlackWebhookEvent" \obj -> do
    tag :: Text <- obj .: "type"
    case tag of
      "url_verification" -> EventUrlVerification <$> parseJSON (Object obj)
      "event_callback" -> EventEventCallback <$> parseJSON (Object obj)
      _ -> pure $ EventUnknownWebhook (Object obj)

-- * Event responses

-- $eventResponses
--
-- By and large, Slack does not care about the response returned from event
-- handlers, at least for the 'EventEventCallback' as long as your service
-- 200s. The exception is 'EventUrlVerification', which is expected to return a
-- 'UrlVerificationResponse'

-- | Response for @url_verification@.
data UrlVerificationResponse = UrlVerificationResponse
  { challenge :: Text
  }
  deriving stock (Show)

$(deriveToJSON defaultOptions ''UrlVerificationResponse)
