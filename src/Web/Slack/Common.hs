{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Common
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Common
  ( Color(..)
  , UserId(..)
  , ConversationId (..)
  , TeamId (..)
  , Cursor (..)
  , SlackTimestamp(..)
  , mkSlackTimestamp
  , timestampFromText
  , HistoryReq(..)
  , mkHistoryReq
  , HistoryRsp(..)
  , Message(..)
  , MessageType(..)
  , SlackClientError(..)
  , SlackMessageText(..)
  )
  where

-- aeson
import Data.Aeson
import Data.Aeson.TH

-- base
import Control.Exception
import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData)

-- http-api-data
import Web.HttpApiData
import Web.FormUrlEncoded

-- servant-client
import Servant.Client

-- slack-web
import Web.Slack.Types
import Web.Slack.Util

-- text
import Data.Text (Text)

#if !MIN_VERSION_servant(0,16,0)
type ClientError = ServantError
#endif

-- |
--
--

data HistoryReq =
  HistoryReq
    { historyReqChannel :: Text
    , historyReqCount :: Int
    , historyReqLatest :: Maybe SlackTimestamp
    , historyReqOldest :: Maybe SlackTimestamp
    , historyReqInclusive :: Bool
    }
  deriving (Eq, Show, Generic)

instance NFData HistoryReq


-- |
--
--

$(deriveJSON (jsonOpts "historyReq") ''HistoryReq)


-- |
--
--

instance ToForm HistoryReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm HistoryReq{..} =
    [ ("channel", toQueryParam historyReqChannel)
    , ("count", toQueryParam historyReqCount)
    ]
      <> toQueryParamIfJust "latest" historyReqLatest
      <> toQueryParamIfJust "oldest" historyReqOldest
      <> [("inclusive", toQueryParam (if historyReqInclusive then 1 :: Int else 0))]


-- |
--
--

mkHistoryReq
  :: Text
  -> HistoryReq
mkHistoryReq channel =
  HistoryReq
    { historyReqChannel = channel
    , historyReqCount = 100
    , historyReqLatest = Nothing
    , historyReqOldest = Nothing
    , historyReqInclusive = True
    }

data MessageType = MessageTypeMessage
  deriving (Eq, Show, Generic)

instance NFData MessageType

instance FromJSON MessageType where
  parseJSON "message" = pure MessageTypeMessage
  parseJSON _ = fail "Invalid MessageType"

instance ToJSON MessageType where
  toJSON _ = String "message"

data Message =
  Message
    { messageType :: MessageType
    , messageUser :: Maybe UserId -- ^ not present for bot messages at least
    , messageText :: SlackMessageText
    -- ^ the message text is in a markdown-like slack-specific format.
    -- Use 'Web.Slack.MessageParser.messageToHtml' to convert it to HTML.
    , messageTs :: SlackTimestamp
    }
  deriving (Eq, Generic, Show)

instance NFData Message

$(deriveJSON (jsonOpts "message") ''Message)

data HistoryRsp =
  HistoryRsp
    { historyRspMessages :: [Message]
    , historyRspHasMore :: Bool
    }
  deriving (Eq, Generic, Show)

instance NFData HistoryRsp

$(deriveJSON (jsonOpts "historyRsp") ''HistoryRsp)

-- |
-- Errors that can be triggered by a slack request.
data SlackClientError
    = ServantError ClientError
    -- ^ errors from the network connection
    | SlackError Text
    -- ^ errors returned by the slack API
  deriving (Eq, Generic, Show)

instance NFData SlackClientError

instance Exception SlackClientError
