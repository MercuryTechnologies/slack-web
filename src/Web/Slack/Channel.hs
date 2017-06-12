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

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)
import Data.Text.Read (decimal)

-- time
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- errors
import Control.Error (hush)

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
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "historyReq") ''HistoryReq)


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
    }


-- |
--
--

data MessageType = MessageTypeMessage
  deriving (Eq, Show)

instance FromJSON MessageType where
  parseJSON "message" = pure MessageTypeMessage
  parseJSON _ = fail "Invalid MessageType"

data SlackTimestamp =
  SlackTimestamp
    { slackTimestampTs :: Text
    , slackTimestampTime :: UTCTime
    } deriving (Eq, Show)

instance FromJSON SlackTimestamp where
  parseJSON = withText "Slack ts" $ \contents ->
    maybe (fail "Invalid slack ts")
          (pure . SlackTimestamp contents)
          (slackTimestampToTime contents)

slackTimestampToTime :: Text -> Maybe UTCTime
slackTimestampToTime txt =
  posixSecondsToUTCTime . realToFrac @Integer . fst <$> hush (decimal txt)

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
    }
  deriving (Eq, Generic, Show)

-- |
--
--
$(deriveFromJSON (jsonOpts "historyRsp") ''HistoryRsp)
