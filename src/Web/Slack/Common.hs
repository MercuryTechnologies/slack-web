{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Common
-- Description:
module Web.Slack.Common (
  Color (..),
  UserId (..),
  ConversationId (..),
  TeamId (..),
  Cursor (..),
  SlackTimestamp (..),
  mkSlackTimestamp,
  timestampFromText,
  Message (..),
  MessageType (..),
  SlackClientError (..),
  SlackMessageText (..),
) where

-- FIXME: Web.Slack.Prelude

-- aeson

-- base

-- deepseq
import Control.DeepSeq (NFData)
import Control.Exception
import Data.Aeson
import Data.Aeson.TH
-- servant-client

-- slack-web

-- text
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Client
import Web.Slack.Types
import Web.Slack.Util
import Prelude

#if !MIN_VERSION_servant(0,16,0)
type ClientError = ServantError
#endif

data MessageType = MessageTypeMessage
  deriving stock (Eq, Show, Generic)

instance NFData MessageType

instance FromJSON MessageType where
  parseJSON "message" = pure MessageTypeMessage
  parseJSON _ = fail "Invalid MessageType"

instance ToJSON MessageType where
  toJSON _ = String "message"

data Message = Message
  { messageType :: MessageType
  , messageUser :: Maybe UserId
  -- ^ not present for bot messages at least
  , messageText :: SlackMessageText
  -- ^ the message text is in a markdown-like slack-specific format.
  -- Use 'Web.Slack.MessageParser.messageToHtml' to convert it to HTML.
  , messageTs :: SlackTimestamp
  }
  deriving stock (Eq, Generic, Show)

instance NFData Message

$(deriveJSON (jsonOpts "message") ''Message)

-- |
-- Errors that can be triggered by a slack request.
data SlackClientError
  = -- | errors from the network connection
    ServantError ClientError
  | -- | errors returned by the slack API
    SlackError Text
  deriving stock (Eq, Generic, Show)

instance NFData SlackClientError

instance Exception SlackClientError
