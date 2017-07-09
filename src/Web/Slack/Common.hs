{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
    ( HistoryReq(..)
    , SlackTimestamp(..)
    , mkSlackTimestamp
    , mkHistoryReq
    , HistoryRsp(..)
    , Message(..)
    , Color(unColor)
    , UserId(unUserId)
    , SlackClientError(..)
    )
    where

-- aeson
import Data.Aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)
import Data.Monoid

-- errors
import Control.Error (hush)

-- http-api-data
import Web.FormUrlEncoded
import Web.HttpApiData

-- servant-client
import Servant.Common.Req

-- slack-web
import Web.Slack.Util
import Web.Slack.MessageParser

-- text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

-- time
import Data.Time.Clock
import Data.Time.Clock.POSIX

newtype Color = Color { unColor :: Text }
  deriving (Eq, Ord, Generic, Show, FromJSON)

newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Generic, Show, FromJSON)

data SlackTimestamp =
  SlackTimestamp
    { slackTimestampTs :: Text
    , slackTimestampTime :: UTCTime
    }
  deriving (Eq, Show)

instance Ord SlackTimestamp where
    compare (SlackTimestamp _ a) (SlackTimestamp _ b) = compare a b

mkSlackTimestamp :: UTCTime -> SlackTimestamp
mkSlackTimestamp utctime = SlackTimestamp (T.pack (show @Integer unixts) <> ".000000") utctime
  where unixts = floor $ toRational $ utcTimeToPOSIXSeconds utctime

instance ToHttpApiData SlackTimestamp where
  toQueryParam (SlackTimestamp contents _) = contents

instance FromJSON SlackTimestamp where
  parseJSON = withText "Slack ts" $ \contents ->
    maybe (fail "Invalid slack ts")
          (pure . SlackTimestamp contents)
          (slackTimestampToTime contents)

slackTimestampToTime :: Text -> Maybe UTCTime
slackTimestampToTime txt =
  posixSecondsToUTCTime . realToFrac @Integer . fst <$> hush (decimal txt)

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
    , messageText :: Text
    , messageAsHtml :: Text -- ^ not provided by slack REST api, slack-web does the conversion
    , messageTs :: SlackTimestamp
    }
  deriving (Eq, Generic, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    Message
        <$> o .: "type"
        <*> o .:? "user"
        <*> o .: "text"
        <*> (messageToHtml <$> o .: "text")
        <*> o .: "ts"

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
  deriving (Eq, Generic, Show)
