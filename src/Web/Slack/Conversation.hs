{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Channel
-- Description: Types and functions related to <https://api.slack.com/docs/conversations-api Conversation API>
--
--
--
----------------------------------------------------------------------

module Web.Slack.Conversation
  ( Conversation(..)
  , ConversationId(..)
  , ConversationType(..)
  , ChannelConversation(..)
  , GroupConversation(..)
  , ImConversation(..)
  , TeamId(..)
  , Purpose(..)
  , Topic(..)
  , ListReq(..)
  , mkListReq
  , ListRsp(..)
  , HistoryRsp(..)
  ) where

-- aeson
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.TH
import Data.Aeson.Types

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- base
import Control.Applicative (empty, (<|>))
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded
import Web.HttpApiData

-- slack-web
import Web.Slack.Common hiding (HistoryRsp)
import Web.Slack.Types
import Web.Slack.Util

-- scientific
import Data.Scientific

-- text
import Data.Text (Text)
import qualified Data.Text as T


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

$(deriveJSON (jsonOpts "topic") ''Topic)


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

$(deriveJSON (jsonOpts "purpose") ''Purpose)


-- | Conversation object representing a public channel,
--   which any people in the team can join in and see.
data ChannelConversation =
  ChannelConversation
    { channelId :: ConversationId
    , channelName :: Text
    , channelCreated :: Integer
    , channelIsArchived :: Bool
    , channelIsGeneral :: Bool
    , channelUnlinked :: Integer
    , channelNameNormalized :: Text
    , channelIsShared :: Bool

    -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , channelParentConversation: null
    , channelCreator :: UserId
    , channelIsExtShared :: Bool
    , channelIsOrgShared :: Bool
    , channelSharedTeamIds :: [TeamId]

    -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , channelPendingShared :: [TeamId]
    -- , channelPendingConnectedTeamIds :: [TeamId]

    , channelIsPendingExtShared :: Bool
    , channelIsMember :: Bool
    , channelTopic :: Topic
    , channelPurpose :: Purpose
    , channelPreviousNames :: [Text]
    , channelNumMembers :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "channel") ''ChannelConversation)


-- | Conversation object representing a private channel or
--   _a multi-party instant message (mpim)*, which only invited people in the
--  team can join in and see.
data GroupConversation =
  GroupConversation
    { groupId :: ConversationId
    , groupName :: Text
    , groupCreated :: Integer
    , groupIsArchived :: Bool
    , groupIsGeneral :: Bool
    , groupUnlinked :: Integer
    , groupNameNormalized :: Text
    , groupIsShared :: Bool

    -- FIXME:
    -- I'm not sure the correct type of this field, because I only found
    -- example responses whose @parent_conversation@ is @null@
    -- , groupParentConversation :: null

    , groupCreator :: UserId
    , groupIsExtShared :: Bool
    , groupIsOrgShared :: Bool
    , groupSharedTeamIds :: [TeamId]

    -- FIXME:
    -- I'm not sure the correct type of these fields, because I only found
    -- example responses whose @pending_connected_team_ids@ and
    -- @pending_shared@ are empty arrays. (Perhaps this is because
    -- my team is a free account. The names make me guess its type is
    -- @[TeamId]@, but these were not documented as long as I looked up.
    -- , group_pending_shared :: []
    -- , group_pending_connected_team_ids :: []

    , groupIsPendingExtShared :: Bool
    , groupIsMember :: Bool
    , groupIsPrivate :: Bool
    , groupIsMpim :: Bool
    , groupLastRead :: SlackTimestamp
    , groupIsOpen :: Bool
    , groupTopic :: Topic
    , groupPurpose :: Purpose
    , groupPriority :: Scientific
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "group") ''GroupConversation)


-- | Conversation object representing a (single-party) instance message,
--   where only two people talk.
data ImConversation =
  ImConversation
    { imId :: ConversationId
    , imCreated :: Integer
    , imIsArchived :: Bool
    , imIsOrgShared :: Bool
    , imUser :: UserId
    , imIsUserDeleted :: Bool
    , imPriority :: Scientific
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "im") ''ImConversation)


-- | Ref. https://api.slack.com/types/conversation
--
--
data Conversation =
      Channel ChannelConversation
    | Group GroupConversation
    | Im ImConversation
  deriving (Eq, Generic, Show)


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
      is <- (o .: key) <|> empty
      if is
        then con <$> parseJSON (Object o)
        else empty


instance ToJSON Conversation where
  toJSON (Channel channel) =
    let (Object obj) = toJSON channel
     in Object
          . HM.insert "is_channel" (Bool True)
          . HM.insert "is_group" (Bool False)
          $ HM.insert "is_im" (Bool False) obj
  toJSON (Group group) =
    let (Object obj) = toJSON group
     in Object
          . HM.insert "is_channel" (Bool False)
          . HM.insert "is_group" (Bool True)
          $ HM.insert "is_im" (Bool False) obj
  toJSON (Im im) =
    let (Object obj) = toJSON im
     in Object
          . HM.insert "is_channel" (Bool False)
          . HM.insert "is_group" (Bool False)
          $ HM.insert "is_im" (Bool True) obj


data ConversationType =
    PublicChannelType
  | PrivateChannelType
  | MpimType
  | ImType
  deriving (Eq, Generic, Show)

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
        . typeMismatch "ConversationType" $ String actual



data ListReq =
  ListReq
    { listReqExcludeArchived :: Maybe Bool
    , listReqTypes :: [ConversationType]
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "listReq") ''ListReq)

-- |
--
--

mkListReq
  :: ListReq
mkListReq =
  ListReq
    { listReqExcludeArchived = Nothing
    , listReqTypes = []
    }


-- |
--
--

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


-- |
--

newtype ListRsp =
  ListRsp
    { listRspChannels :: [Conversation]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)


-- |
--

data HistoryRsp =
  HistoryRsp
    { historyRspMessages :: [Message]
    , historyRspHasMore :: Bool
    , historyRspPinCount :: Integer
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "historyRsp") ''HistoryRsp)
