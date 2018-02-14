{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Common
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Common
  ( Color(unColor)
  , UserId(unUserId)
  , SlackTimestamp(..)
  , mkSlackTimestamp
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
import Data.Typeable
import GHC.Generics (Generic)

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
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveFromJSON (jsonOpts "historyReq") ''HistoryReq)


-- |
--
--

instance ToForm HistoryReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm HistoryReq{..} =
      [ ("channel", toQueryParam historyReqChannel)
      , ("count", toQueryParam historyReqCount)
      , ("latest", toQueryParam historyReqLatest)
      , ("oldest", toQueryParam historyReqOldest)
      , ("inclusive", toQueryParam (if historyReqInclusive then 1::Int else 0))
      ]


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
  deriving (Eq, Show)

instance FromJSON MessageType where
  parseJSON "message" = pure MessageTypeMessage
  parseJSON _ = fail "Invalid MessageType"

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

$(deriveFromJSON (jsonOpts "message") ''Message)

data HistoryRsp =
  HistoryRsp
    { historyRspMessages :: [Message]
    , historyRspHasMore :: Bool
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "historyRsp") ''HistoryRsp)

-- |
-- Errors that can be triggered by a slack request.
data SlackClientError
    = ServantError ServantError
    -- ^ errors from the network connection
    | SlackError Text
    -- ^ errors returned by the slack API
  deriving (Eq, Generic, Show, Typeable)

instance Exception SlackClientError
