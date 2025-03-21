{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- FIXME: squashes warnings
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Channel
-- Description: Types and functions related to <https://api.slack.com/docs/conversations-api Conversation API>
module Web.Slack.Conversation (
  Conversation (..),
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
  Api,
  InfoReq (..),
  InfoRsp (..),
  conversationsInfo,
  conversationsInfo_,
) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Scientific
import Data.Text qualified as T
import Servant.API (AuthProtect, FormUrlEncoded, JSON, Post, ReqBody, (:>))
import Servant.Client (ClientM, client)
import Servant.Client.Core (AuthenticatedRequest)
import Web.FormUrlEncoded
import Web.HttpApiData
import Web.Slack.Common
import Web.Slack.Internal (ResponseJSON (..), SlackConfig (..), mkSlackAuthenticateReq, run)
import Web.Slack.Pager (Response)
import Web.Slack.Pager.Types (PagedRequest (..), PagedResponse (..), ResponseMetadata (..))
import Web.Slack.Prelude
import Web.Slack.Util

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
  , channelSharedTeamIds :: Maybe [TeamId]
  -- ^ Ironically this has been observed to be absent on real shared-channel
  -- responses.
  , -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , channelPendingShared :: [TeamId]
    -- , channelPendingConnectedTeamIds :: [TeamId]

    channelIsPendingExtShared :: Bool
  , channelIsMember :: Maybe Bool
  -- ^ Absent from @users.conversations@ response
  , channelTopic :: Topic
  , channelPurpose :: Purpose
  , channelPreviousNames :: [Text]
  , channelNumMembers :: Maybe Integer
  -- ^ Absent from @conversations.join@ response
  }
  deriving stock (Eq, Show, Generic)

instance NFData ChannelConversation

$(deriveJSON (jsonOpts "channel") ''ChannelConversation)

-- | Conversation object representing a private channel or
--   _a multi-party instant message (mpim)_, which only invited people in the
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
  , imIsUserDeleted :: Maybe Bool
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
    fromMaybe (noneMatched o)
      =<< parseWhen "is_channel" Channel o
      `parseOr` parseWhen "is_group" Group o
      `parseOr` parseWhen "is_im" Im o
    where
      noneMatched o =
        prependFailure
          "parsing a Conversation failed: neither channel, group, nor im: "
          (typeMismatch "Conversation" (Object o))

      -- '(<|>)' that pierces one layer of 'Monad' first
      parseOr :: (Monad m, Alternative a) => m (a b) -> m (a b) -> m (a b)
      parseOr = liftM2 (<|>)

      -- This uses the outer Parser monad since deciding which parser to use
      -- is monadic, then the Maybe to decide which parser is picked, then
      -- finally the inner parser to actually run it
      parseWhen :: (FromJSON a) => Key -> (a -> b) -> Object -> Parser (Maybe (Parser b))
      parseWhen key con o = do
        -- Slack only inconsistently includes the is_* attributes if false.
        is <- o .:? key .!= False
        if is
          then pure . Just $ con <$> parseJSON (Object o)
          else pure $ Nothing

instance ToJSON Conversation where
  toJSON (Channel channel) =
    let (Object obj) = toJSON channel
     in Object
          . KM.insert "is_channel" (Bool True)
          . KM.insert "is_group" (Bool False)
          $ KM.insert "is_im" (Bool False) obj
  toJSON (Group theGroup) =
    let (Object obj) = toJSON theGroup
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
  , listReqCursor :: Maybe Cursor
  , listReqLimit :: Maybe Int
  , listReqTeamId :: Maybe TeamId
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
    , listReqLimit = Nothing
    , listReqTeamId = Nothing
    , listReqCursor = Nothing
    }

instance ToForm ListReq where
  toForm
    ( ListReq
        { listReqExcludeArchived
        , listReqTypes = types
        , listReqTeamId
        , listReqCursor
        , listReqLimit
        }
      ) =
      archivedForm
        <> typesForm
        <> toQueryParamIfJust "team_id" listReqTeamId
        <> toQueryParamIfJust "cursor" listReqCursor
        <> toQueryParamIfJust "limit" listReqLimit
      where
        archivedForm =
          maybe mempty (\val -> [("exclude_archived", toUrlPiece val)]) listReqExcludeArchived
        typesForm =
          if null types
            then mempty
            else [("types", T.intercalate "," $ map toUrlPiece types)]

data ListRsp = ListRsp
  { listRspChannels :: [Conversation]
  , listRspResponseMetadata :: Maybe ResponseMetadata
  }
  deriving stock (Eq, Show, Generic)

instance NFData ListRsp

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)

instance PagedRequest ListReq where
  setCursor c r = r {listReqCursor = c}

instance PagedResponse ListRsp where
  type ResponseObject ListRsp = Conversation
  getResponseData ListRsp {listRspChannels} = listRspChannels
  getResponseMetadata ListRsp {listRspResponseMetadata} = listRspResponseMetadata

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

data HistoryRsp = HistoryRsp
  { historyRspMessages :: [Message]
  , historyRspResponseMetadata :: Maybe ResponseMetadata
  }
  deriving stock (Eq, Show, Generic)

instance NFData HistoryRsp

$(deriveJSON (jsonOpts "historyRsp") ''HistoryRsp)

instance PagedRequest HistoryReq where
  setCursor c r = r {historyReqCursor = c}

instance PagedResponse HistoryRsp where
  type ResponseObject HistoryRsp = Message
  getResponseMetadata HistoryRsp {historyRspResponseMetadata} = historyRspResponseMetadata
  getResponseData HistoryRsp {historyRspMessages} = historyRspMessages

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

instance PagedRequest RepliesReq where
  setCursor c r = r {repliesReqCursor = c}

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

-- | @conversations.info@ request: retrieve a conversation's metadata.
--
-- <https://api.slack.com/methods/conversations.info>
--
-- @since 2.2.0.0
data InfoReq = InfoReq
  { infoReqChannel :: ConversationId
  , infoReqIncludeLocale :: Maybe Bool
  , infoReqIncludeNumMembers :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToForm InfoReq where
  toForm InfoReq {..} =
    [("channel", unConversationId infoReqChannel)]
      <> toQueryParamIfJust "include_locale" infoReqIncludeLocale
      <> toQueryParamIfJust "include_num_members" infoReqIncludeNumMembers

-- | @conversations.info@ response
--
-- <https://api.slack.com/methods/conversations.info>
--
-- @since 2.2.0.0
data InfoRsp = InfoRsp
  { infoRspChannel :: Conversation
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON (jsonOpts "infoRsp") ''InfoRsp)

-- | FIXME(jadel): move the rest of the Conversations API into here since the old "shoving all the API in one spot" is soft deprecated.
-- @since 2.2.0.0
type Api =
  "conversations.info"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] InfoReq
    :> Post '[JSON] (ResponseJSON InfoRsp)

-- | Retrieve a conversation's metadata.
--
-- <https://api.slack.com/methods/conversations.info>
--
-- @since 2.2.0.0
conversationsInfo ::
  SlackConfig ->
  InfoReq ->
  IO (Response InfoRsp)
conversationsInfo = flip $ \listReq -> do
  authR <- mkSlackAuthenticateReq
  run (conversationsInfo_ authR listReq) . slackConfigManager

-- | @since 2.2.0.0
conversationsInfo_ ::
  AuthenticatedRequest (AuthProtect "token") ->
  InfoReq ->
  ClientM (ResponseJSON InfoRsp)
conversationsInfo_ = client (Proxy @Api)
