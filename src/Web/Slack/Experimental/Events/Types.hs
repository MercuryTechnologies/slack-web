{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- FIXME(jadel): Use NoFieldSelectors when we drop everything before 9.2.

-- | Types for the [Slack Events API](https://api.slack.com/events).
module Web.Slack.Experimental.Events.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Scientific qualified as Sci
import Data.Time.Clock.System (SystemTime)
import Web.Slack.AesonUtils
import Web.Slack.Experimental.Blocks (SlackBlock)
import Web.Slack.Files.Types (FileObject)
import Web.Slack.Prelude
import Web.Slack.Types (ConversationId, TeamId, UserId)

-- | Not a ConversationType for some mysterious reason; this one has Channel as
-- an option, which does not exist as a ConversationType. Slack, why?
data ChannelType = Channel | Group | Im
  deriving stock (Show, Eq)

$(deriveJSON snakeCaseOptions ''ChannelType)

-- | <https://api.slack.com/events/message/message_attachment>
-- Ported from https://github.com/slackapi/node-slack-sdk/blob/fc87d51/packages/types/src/message-attachments.ts
--
-- @since 2.0.0.3
data AttachmentField = AttachmentField
  { title :: Text
  , value :: Text
  , short :: Maybe Bool
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''AttachmentField)

-- | @since 2.0.0.3
data AttachmentMessageBlockMessage = AttachmentMessageBlockMessage
  { blocks :: [SlackBlock]
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''AttachmentMessageBlockMessage)

-- | @since 2.0.0.3
data AttachmentMessageBlock = AttachmentMessageBlock
  { team :: TeamId
  , channel :: ConversationId
  , ts :: Text
  , message :: AttachmentMessageBlockMessage
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''AttachmentMessageBlock)

-- | <https://api.slack.com/events/message/message_attachment>
-- Ported from https://github.com/slackapi/node-slack-sdk/blob/fc87d51/packages/types/src/message-attachments.ts
--
-- @since 2.0.0.3
data DecodedMessageAttachment = DecodedMessageAttachment
  { fallback :: Maybe Text
  , color :: Maybe Text
  , pretext :: Maybe Text
  , authorName :: Maybe Text
  , authorLink :: Maybe Text
  , authorIcon :: Maybe Text
  , title :: Maybe Text
  , titleLink :: Maybe Text
  , text :: Maybe Text
  , fields :: Maybe [AttachmentField]
  , imageUrl :: Maybe Text
  , thumbUrl :: Maybe Text
  , footer :: Maybe Text
  , footerIcon :: Maybe Text
  , ts :: Maybe Text
  , -- the following are undocumented
    isMsgUnfurl :: Maybe Bool
  , messageBlocks :: Maybe [AttachmentMessageBlock]
  -- ^ unfurled message blocks
  , authorId :: Maybe UserId
  , channelId :: Maybe ConversationId
  , channelTeam :: Maybe TeamId
  , isAppUnfurl :: Maybe Bool
  , appUnfurlUrl :: Maybe Text
  , fromUrl :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON DecodedMessageAttachment where
  parseJSON = withObject "DecodedMessageAttachment" $ \v -> do
    fallback <- v .:? "fallback"
    color <- v .:? "color"
    pretext <- v .:? "pretext"
    authorName <- v .:? "author_name"
    authorLink <- v .:? "author_link"
    authorIcon <- v .:? "author_icon"
    title <- v .:? "title"
    titleLink <- v .:? "title_link"
    text <- v .:? "text"
    fields <- v .:? "fields"
    imageUrl <- v .:? "image_url"
    thumbUrl <- v .:? "thumb_url"
    footer <- v .:? "footer"
    footerIcon <- v .:? "footer_icon"
    ts <- v .:? "ts" >>= maybe (return Nothing) parseTs
    isMsgUnfurl <- v .:? "is_msg_unfurl"
    messageBlocks <- v .:? "message_blocks"
    authorId <- v .:? "author_id"
    channelId <- v .:? "channel_id"
    channelTeam <- v .:? "channel_team"
    isAppUnfurl <- v .:? "is_app_unfurl"
    appUnfurlUrl <- v .:? "app_unfurl_url"
    fromUrl <- v .:? "from_url"
    pure DecodedMessageAttachment {..}
    where
      parseTs :: Value -> Parser (Maybe Text)
      parseTs (String s) = pure $ Just s
      parseTs (Number n) =
        let s = Sci.formatScientific Sci.Fixed Nothing n
            formatted = if '.' `elem` s then s else s ++ ".000000"
         in pure $ Just (pack formatted)
      parseTs _ = pure Nothing

data MessageAttachment = MessageAttachment
  { decoded :: Maybe DecodedMessageAttachment
  -- ^ If the attachment can be decoded, this will be populated
  , raw :: Value
  -- ^ Slack does not document the attachment schema/spec very well and we can't
  -- decode many attachments. In these cases clients can work with the raw JSON.
  }
  deriving stock (Show)

instance FromJSON MessageAttachment where
  parseJSON = withObject "MessageAttachment" $ \v -> do
    let ov = Object v
    -- Attempt to parse the entire object as DecodedMessageAttachment
    decodedContent <- optional $ parseJSON ov
    -- Return the structured data with raw JSON preserved
    pure MessageAttachment {decoded = decodedContent, raw = ov}

-- | <https://api.slack.com/events/message>
-- and
-- <https://api.slack.com/events/message/file_share>
data MessageEvent = MessageEvent
  { blocks :: Maybe [SlackBlock]
  , channel :: ConversationId
  , text :: Text
  , channelType :: ChannelType
  , files :: Maybe [FileObject]
  -- ^ @since 1.6.0.0
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
  , attachments :: Maybe [MessageAttachment]
  -- ^ @since 2.0.0.3
  -- Present if the message has attachments
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''MessageEvent)

-- | <https://api.slack.com/events/message/bot_message>
-- This is similar to a MessageEvent but sent by a bot, for example
-- messages that Reacji Channeler sends.
--
-- @since 2.0.0.2
data BotMessageEvent = BotMessageEvent
  { blocks :: Maybe [SlackBlock]
  , channel :: ConversationId
  , text :: Text
  , channelType :: ChannelType
  , files :: Maybe [FileObject]
  , ts :: Text
  , threadTs :: Maybe Text
  -- ^ Present if the message is in a thread
  , appId :: Maybe Text
  -- ^ Some (or all) bots also have an App ID
  , botId :: Text
  -- ^ Always present for bot_message subtype
  , attachments :: Maybe [MessageAttachment]
  -- ^ @since 2.0.0.3
  -- Present if the message has attachments
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''BotMessageEvent)

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
  | EventBotMessage BotMessageEvent
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
  deriving stock (Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "MessageEvent" \obj -> do
    tag :: Text <- obj .: "type"
    subtype :: Maybe Text <- obj .:? "subtype"
    case (tag, subtype) of
      ("message", Nothing) -> EventMessage <$> parseJSON @MessageEvent (Object obj)
      ("message", Just "bot_message") -> EventBotMessage <$> parseJSON @BotMessageEvent (Object obj)
      ("message", Just "message_changed") -> pure EventMessageChanged
      ("message", Just "channel_join") -> pure EventChannelJoinMessage
      -- n.b. these are unified since it is *identical* to a Message event with
      -- a bonus files field
      ("message", Just "file_share") -> EventMessage <$> parseJSON @MessageEvent (Object obj)
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
  deriving stock (Show, Generic)

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
