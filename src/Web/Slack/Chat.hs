{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Chat (
  PostMsg (..),
  PostMsgReq (..),
  mkPostMsgReq,
  PostMsgRsp (..),
  UpdateReq (..),
  mkUpdateReq,
  UpdateRsp (..),
) where

import Web.FormUrlEncoded
import Web.Slack.Conversation (ConversationId)
import Web.Slack.Prelude
import Web.Slack.Util

data PostMsg = PostMsg
  { postMsgText :: Text
  , postMsgParse :: Maybe Text
  , postMsgLinkNames :: Maybe Bool
  , postMsgAttachments :: Maybe Text
  , postMsgUnfurlLinks :: Maybe Bool
  , postMsgUnfurlMedia :: Maybe Bool
  , postMsgUsername :: Maybe Text
  , postMsgAsUser :: Maybe Bool
  , postMsgIconUrl :: Maybe Text
  , postMsgIconEmoji :: Maybe Text
  , postMsgThreadTs :: Maybe Text
  , postMsgReplyBroadcast :: Maybe Bool
  }
  deriving stock (Eq, Generic, Show)

instance NFData PostMsg

$(deriveJSON (jsonOpts "postMsg") ''PostMsg)

data PostMsgReq = PostMsgReq
  { postMsgReqChannel :: Text
  , postMsgReqText :: Maybe Text
  -- ^ One of 'postMsgReqText', 'postMsgReqAttachments', or 'postMsgReqBlocks'
  -- is required.
  , postMsgReqParse :: Maybe Text
  , postMsgReqLinkNames :: Maybe Bool
  , postMsgReqAttachments :: Maybe Text
  , postMsgReqBlocks :: Maybe Text
  , postMsgReqUnfurlLinks :: Maybe Bool
  , postMsgReqUnfurlMedia :: Maybe Bool
  , postMsgReqUsername :: Maybe Text
  , postMsgReqAsUser :: Maybe Bool
  , postMsgReqIconUrl :: Maybe Text
  , postMsgReqIconEmoji :: Maybe Text
  , postMsgReqThreadTs :: Maybe Text
  , postMsgReqReplyBroadcast :: Maybe Bool
  }
  deriving stock (Eq, Generic, Show)

instance NFData PostMsgReq

$(deriveJSON (jsonOpts "postMsgReq") ''PostMsgReq)

instance ToForm PostMsgReq where
  toForm =
    genericToForm (formOpts "postMsgReq")

mkPostMsgReq ::
  Text ->
  Text ->
  PostMsgReq
mkPostMsgReq channel text =
  PostMsgReq
    { postMsgReqChannel = channel
    , postMsgReqText = Just text
    , postMsgReqParse = Nothing
    , postMsgReqLinkNames = Nothing
    , postMsgReqAttachments = Nothing
    , postMsgReqBlocks = Nothing
    , postMsgReqUnfurlLinks = Nothing
    , postMsgReqUnfurlMedia = Nothing
    , postMsgReqUsername = Nothing
    , postMsgReqAsUser = Nothing
    , postMsgReqIconUrl = Nothing
    , postMsgReqIconEmoji = Nothing
    , postMsgReqThreadTs = Nothing
    , postMsgReqReplyBroadcast = Nothing
    }

data PostMsgRsp = PostMsgRsp
  { postMsgRspTs :: Text
  , postMsgRspMessage :: PostMsg
  }
  deriving stock (Eq, Generic, Show)

instance NFData PostMsgRsp

$(deriveFromJSON (jsonOpts "postMsgRsp") ''PostMsgRsp)

-- | <https://api.slack.com/methods/chat.update>
data UpdateReq = UpdateReq
  { updateReqChannel :: ConversationId
  , updateReqTs :: Text
  -- ^ \"Timestamp of the message to be updated.\"
  , updateReqAsUser :: Maybe Bool
  -- ^ \"Pass true to update the message as the authed user. Bot users in this context are considered authed users.\"
  , updateReqAttachments :: Maybe Text
  -- ^ \"A JSON-based array of structured attachments, presented as a URL-encoded string. This field is required when not presenting text. If you don't include this field, the message's previous attachments will be retained. To remove previous attachments, include an empty array for this field.\"
  , updateReqLinkNames :: Maybe Bool
  , updateReqMetadata :: Maybe Text
  , updateReqParse :: Maybe Text
  , updateReqReplyBroadcast :: Maybe Bool
  -- ^ \"Broadcast an existing thread reply to make it visible to everyone in the channel or conversation.\"
  , updateReqText :: Maybe Text
  -- ^ \"New text for the message, using the default formatting rules. It's not required when presenting blocks or attachments.\"
  }
  deriving stock (Eq, Generic, Show)

instance ToForm UpdateReq where
  toForm = genericToForm (formOpts "updateReq")

mkUpdateReq :: ConversationId -> Text -> UpdateReq
mkUpdateReq channel ts =
  UpdateReq
    { updateReqChannel = channel
    , updateReqTs = ts
    , updateReqAsUser = Nothing
    , updateReqAttachments = Nothing
    , updateReqLinkNames = Nothing
    , updateReqMetadata = Nothing
    , updateReqParse = Nothing
    , updateReqReplyBroadcast = Nothing
    , updateReqText = Nothing
    }

data UpdateRsp = UpdateRsp
  { updateRspChannel :: ConversationId
  , updateRspTs :: Text
  , updateRspText :: Text
  -- FIXME(jadel): this does look suspiciously like the same schema as
  -- MessageEvent based on the example I received, but Slack hasn't documented
  -- what it actually is, so let's not try to parse it for now.
  -- , message :: MessageEvent
  }
  deriving stock (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "updateRsp") ''UpdateRsp)
