{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
-- FIXME: squashes warnings
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Channel
-- Description: Types and functions related to <https://api.slack.com/docs/conversations-api Conversation API>
module Web.Slack.Conversation
  ( Conversation (..),
    ConversationId (..),
    ConversationType (..),
    ChannelConversation (..),
    GroupConversation (..),
    ImConversation (..),
    TeamId (..),
    Purpose (..),
    Topic (..),
    ListReq (..),
    mkListReq,
    ListRsp (..),
    HistoryReq (..),
    mkHistoryReq,
    HistoryRsp (..),
    RepliesReq (..),
    mkRepliesReq,
    ResponseMetadata (..),
  )
where

-- FIXME: Web.Slack.Prelude

-- aeson

-- base
import Control.Applicative (empty, (<|>))
-- deepseq
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types
-- http-api-data

-- slack-web

-- scientific
import Data.Scientific
-- text
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Web.FormUrlEncoded
import Web.HttpApiData
import Web.Slack.Common
import Web.Slack.Util
import Prelude

data Topic = Topic
  { topicValue :: Text
  , topicCreator :: Text
  , topicLastSet :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance NFData Topic

$(deriveJSON (jsonOpts "topic") ''Topic)

data Purpose = Purpose
  { purposeValue :: Text
  , purposeCreator :: Text
  , purposeLastSet :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance NFData Purpose

$(deriveJSON (jsonOpts "purpose") ''Purpose)

-- | Conversation object representing a public channel,
--   which any people in the team can join in and see.
data ChannelConversation = ChannelConversation
  { channelId :: ConversationId
  , channelName :: Text
  , channelCreated :: Integer
  , channelIsArchived :: Bool
  , channelIsGeneral :: Bool
  , channelUnlinked :: Integer
  , channelNameNormalized :: Text
  , channelIsShared :: Bool
  , -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , channelParentConversation: null
    channelCreator :: UserId
  , channelIsExtShared :: Bool
  , channelIsOrgShared :: Bool
  , channelSharedTeamIds :: [TeamId]
  , -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , channelPendingShared :: [TeamId]
    -- , channelPendingConnectedTeamIds :: [TeamId]

    channelIsPendingExtShared :: Bool
  , channelIsMember :: Bool
  , channelTopic :: Topic
  , channelPurpose :: Purpose
  , channelPreviousNames :: [Text]
  , channelNumMembers :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance NFData ChannelConversation

$(deriveJSON (jsonOpts "channel") ''ChannelConversation)

-- | Conversation object representing a private channel or
--   _a multi-party instant message (mpim)*, which only invited people in the
--  team can join in and see.
data GroupConversation = GroupConversation
  { groupId :: ConversationId
  , groupName :: Text
  , groupCreated :: Integer
  , groupIsArchived :: Bool
  , groupIsGeneral :: Bool
  , groupUnlinked :: Integer
  , groupNameNormalized :: Text
  , groupIsShared :: Bool
  , -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , groupParentConversation :: null

    groupCreator :: UserId
  , groupIsExtShared :: Bool
  , groupIsOrgShared :: Bool
  , groupSharedTeamIds :: [TeamId]
  , -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , group_pending_shared :: []
    -- , group_pending_connected_team_ids :: []

    groupIsPendingExtShared :: Bool
  , groupIsMember :: Bool
  , groupIsPrivate :: Bool
  , groupIsMpim :: Bool
  , groupLastRead :: SlackTimestamp
  , groupIsOpen :: Bool
  , groupTopic :: Topic
  , groupPurpose :: Purpose
  , groupPriority :: Scientific
  }
  deriving stock (Eq, Show, Generic)

instance NFData GroupConversation

$(deriveJSON (jsonOpts "group") ''GroupConversation)

-- | Conversation object representing a (single-party) instance message,
--   where only two people talk.
data ImConversation = ImConversation
  { imId :: ConversationId
  , imCreated :: Integer
  , imIsArchived :: Bool
  , imIsOrgShared :: Bool
  , imUser :: UserId
  , imIsUserDeleted :: Bool
  , imPriority :: Scientific
  }
  deriving stock (Eq, Show, Generic)

instance NFData ImConversation

$(deriveJSON (jsonOpts "im") ''ImConversation)

-- | Ref. https://api.slack.com/types/conversation
data Conversation
  = Channel ChannelConversation
  | Group GroupConversation
  | Im ImConversation
  deriving stock (Eq, Show, Generic)

instance NFData Conversation

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    parseWhen "is_channel" Channel o
      <|> parseWhen "is_group" Group o
      <|> parseWhen "is_im" Im o
      <|> prependFailure
        "parsing a Conversation failed: neither channel, group, nor im, "
        (typeMismatch "Conversation" (Object o))
    where
      parseWhen key con o = do
        is <- (o .: key)
        if is
          then con <$> parseJSON (Object o)
          else empty

instance ToJSON Conversation where
  toJSON (Channel channel) =
    let (Object obj) = toJSON channel
     in Object
          . KM.insert "is_channel" (Bool True)
          . KM.insert "is_group" (Bool False)
          $ KM.insert "is_im" (Bool False) obj
  toJSON (Group group) =
    let (Object obj) = toJSON group
     in Object
          . KM.insert "is_channel" (Bool False)
          . KM.insert "is_group" (Bool True)
          $ KM.insert "is_im" (Bool False) obj
  toJSON (Im im) =
    let (Object obj) = toJSON im
     in Object
          . KM.insert "is_channel" (Bool False)
          . KM.insert "is_group" (Bool False)
          $ KM.insert "is_im" (Bool True) obj

data ConversationType
  = PublicChannelType
  | PrivateChannelType
  | MpimType
  | ImType
  deriving stock (Eq, Show, Generic)

instance NFData ConversationType

instance ToHttpApiData ConversationType where
  toUrlPiece PublicChannelType = "public_channel"
  toUrlPiece PrivateChannelType = "private_channel"
  toUrlPiece MpimType = "mpim"
  toUrlPiece ImType = "im"

instance ToJSON ConversationType where
  toJSON = toJSON . toUrlPiece
  toEncoding = text . toUrlPiece

instance FromJSON ConversationType where
  parseJSON = withText "ConversationType" $ \case
    "public_channel" -> pure PublicChannelType
    "private_channel" -> pure PrivateChannelType
    "mpim" -> pure MpimType
    "im" -> pure ImType
    actual ->
      prependFailure "must be either \"public_channel\", \"private_channel\", \"mpim\" or \"im\"!"
        . typeMismatch "ConversationType"
        $ String actual

data ListReq = ListReq
  { listReqExcludeArchived :: Maybe Bool
  , listReqTypes :: [ConversationType]
  }
  deriving stock (Eq, Show, Generic)

instance NFData ListReq

$(deriveJSON (jsonOpts "listReq") ''ListReq)

mkListReq ::
  ListReq
mkListReq =
  ListReq
    { listReqExcludeArchived = Nothing
    , listReqTypes = []
    }

instance ToForm ListReq where
  toForm (ListReq archived types) =
    archivedForm <> typesForm
    where
      archivedForm =
        maybe mempty (\val -> [("archived", toUrlPiece val)]) archived
      typesForm =
        if null types
          then mempty
          else [("types", T.intercalate "," $ map toUrlPiece types)]

newtype ListRsp = ListRsp
  { listRspChannels :: [Conversation]
  }
  deriving stock (Eq, Show, Generic)

instance NFData ListRsp

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)

data HistoryReq = HistoryReq
  { historyReqChannel :: ConversationId
  , historyReqCursor :: Maybe Cursor
  , historyReqCount :: Int
  , historyReqLatest :: Maybe SlackTimestamp
  , historyReqOldest :: Maybe SlackTimestamp
  , historyReqInclusive :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance NFData HistoryReq

$(deriveJSON (jsonOpts "historyReq") ''HistoryReq)

mkHistoryReq ::
  ConversationId ->
  HistoryReq
mkHistoryReq channel =
  HistoryReq
    { historyReqChannel = channel
    , historyReqCursor = Nothing
    , historyReqCount = 100
    , historyReqLatest = Nothing
    , historyReqOldest = Nothing
    , historyReqInclusive = True
    }

instance ToForm HistoryReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm HistoryReq {..} =
    [("channel", toQueryParam historyReqChannel)]
      <> toQueryParamIfJust "cursor" historyReqCursor
      <> [("count", toQueryParam historyReqCount)]
      <> toQueryParamIfJust "latest" historyReqLatest
      <> toQueryParamIfJust "oldest" historyReqOldest
      <> [("inclusive", toQueryParam (if historyReqInclusive then 1 :: Int else 0))]

newtype ResponseMetadata = ResponseMetadata {responseMetadataNextCursor :: Maybe Cursor}
  deriving stock (Eq, Show, Generic)

instance NFData ResponseMetadata

$(deriveJSON (jsonOpts "responseMetadata") ''ResponseMetadata)

data HistoryRsp = HistoryRsp
  { historyRspMessages :: [Message]
  , historyRspResponseMetadata :: Maybe ResponseMetadata
  }
  deriving stock (Eq, Show, Generic)

instance NFData HistoryRsp

$(deriveJSON (jsonOpts "historyRsp") ''HistoryRsp)

data RepliesReq = RepliesReq
  { repliesReqTs :: SlackTimestamp
  , repliesReqCursor :: Maybe Cursor
  , repliesReqChannel :: ConversationId
  , repliesReqLimit :: Int
  , repliesReqLatest :: Maybe SlackTimestamp
  , repliesReqOldest :: Maybe SlackTimestamp
  , repliesReqInclusive :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance NFData RepliesReq

$(deriveJSON (jsonOpts "repliesReq") ''RepliesReq)

instance ToForm RepliesReq where
  -- can't use genericToForm because slack expects booleans as 0/1
  toForm RepliesReq {..} =
    [("channel", toQueryParam repliesReqChannel)]
      <> [("ts", toQueryParam repliesReqTs)]
      <> toQueryParamIfJust "cursor" repliesReqCursor
      <> [("limit", toQueryParam repliesReqLimit)]
      <> toQueryParamIfJust "latest" repliesReqLatest
      <> toQueryParamIfJust "oldest" repliesReqOldest
      <> [("inclusive", toQueryParam (if repliesReqInclusive then 1 :: Int else 0))]

mkRepliesReq ::
  ConversationId ->
  SlackTimestamp ->
  RepliesReq
mkRepliesReq channel ts =
  RepliesReq
    { repliesReqChannel = channel
    , repliesReqCursor = Nothing
    , repliesReqTs = ts
    , repliesReqLimit = 100
    , repliesReqLatest = Nothing
    , repliesReqOldest = Nothing
    , repliesReqInclusive = True
    }
