{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Channel
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Channel
  ( Channel(..)
  , Purpose(..)
  , Topic(..)
  , CreateReq(..)
  , mkCreateReq
  , CreateRsp(..)
  , ListReq(..)
  , mkListReq
  , ListRsp(..)
  , HistoryReq(..)
  , SlackTimestamp(..)
  , mkSlackTimestamp
  , mkHistoryReq
  , HistoryRsp(..)
  , Message(..)
  )
  where

-- aeson
import Data.Aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)
import Data.Monoid

-- http-api-data
import Web.FormUrlEncoded
import Web.HttpApiData

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

-- time
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- errors
import Control.Error (hush)

data SlackTimestamp =
  SlackTimestamp
    { slackTimestampTs :: Text
    , slackTimestampTime :: UTCTime
    } deriving (Eq, Show)

mkSlackTimestamp :: UTCTime -> SlackTimestamp
mkSlackTimestamp utctime = SlackTimestamp (T.pack (show unixts) <> ".000000") utctime
  where unixts = utcTimeToPOSIXSeconds utctime

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

data Channel =
  Channel
    { channelId :: Text
    , channelName :: Text
    , channelCreated :: Integer
    , channelCreator :: Text
    , channelIsArchived :: Bool
    , channelIsMember :: Bool
    , channelIsGeneral :: Bool
    , channelLastRead :: Maybe Text
    , channelLatest :: Maybe Text
    , channelUnreadCount :: Maybe Integer
    , channelUnreadCountDisplay :: Maybe Integer
    , channelMembers :: [Text]
    , channelTopic :: Topic
    , channelPurpose :: Purpose
    }
  deriving (Eq, Generic, Show)


-- |
--
--

data Purpose =
  Purpose
    { purposeValue :: Text
    , purposeCreator :: Text
    , purposeLastSet :: Integer
    }
  deriving (Eq, Generic, Show)


-- |
--
--

data Topic =
  Topic
    { topicValue :: Text
    , topicCreator :: Text
    , topicLastSet :: Integer
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "channel") ''Channel)


-- |
--
--

$(deriveJSON (jsonOpts "purpose") ''Purpose)


-- |
--
--

$(deriveJSON (jsonOpts "topic") ''Topic)


-- |
--
--

data CreateReq =
  CreateReq
    { createReqName :: Text
    , createReqValidate :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "createReq") ''CreateReq)


-- |
--
--

instance ToForm CreateReq where
  toForm =
    genericToForm (formOpts "createReq")


-- |
--
--

mkCreateReq
  :: Text
  -> CreateReq
mkCreateReq name =
  CreateReq
    { createReqName = name
    , createReqValidate = Nothing
    }


-- |
--
--

data CreateRsp =
  CreateRsp
    { createRspOk :: Bool
    , createRspChannel :: Channel
    }
  deriving (Eq, Generic, Show)


-- |
--
--
$(deriveJSON (jsonOpts "createRsp") ''CreateRsp)

-- |
--
--

data ListReq =
  ListReq
    { listReqExcludeArchived :: Maybe Bool
    , listReqExcludeMembers :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "listReq") ''ListReq)


-- |
--
--

instance ToForm ListReq where
  toForm =
    genericToForm (formOpts "listReq")


-- |
--
--

mkListReq
  :: ListReq
mkListReq =
  ListReq
    { listReqExcludeArchived = Nothing
    , listReqExcludeMembers = Nothing
    }


-- |
--
--

data ListRsp =
  ListRsp
    { listRspOk :: Bool
    , listRspChannels :: [Channel]
    }
  deriving (Eq, Generic, Show)


-- |
--
--
$(deriveJSON (jsonOpts "listRsp") ''ListRsp)

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
  toForm =
    genericToForm (formOpts "historyReq")


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


-- |
--
--

data MessageType = MessageTypeMessage
  deriving (Eq, Show)

instance FromJSON MessageType where
  parseJSON "message" = pure MessageTypeMessage
  parseJSON _ = fail "Invalid MessageType"

data Message =
  Message
    { messageType :: MessageType
    , messageUser :: Maybe Text -- not present for bot messages at least
    , messageText :: Text
    , messageTs :: SlackTimestamp
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "message") ''Message)

data HistoryRsp =
  HistoryRsp
    { historyRspOk :: Bool
    , historyRspMessages :: [Message]
    , historyRspHasMore :: Bool
    }
  deriving (Eq, Generic, Show)

-- |
--
--
$(deriveFromJSON (jsonOpts "historyRsp") ''HistoryRsp)
