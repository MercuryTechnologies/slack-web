{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Chat
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Chat
  ( PostMsg(..)
  , PostMsgReq(..)
  , mkPostMsgReq
  , PostMsgRsp(..)
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


data PostMsg =
  PostMsg
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
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "postMsg") ''PostMsg)


-- |
--
--

data PostMsgReq =
  PostMsgReq
    { postMsgReqChannel :: Text
    , postMsgReqText :: Text
    , postMsgReqParse :: Maybe Text
    , postMsgReqLinkNames :: Maybe Bool
    , postMsgReqAttachments :: Maybe Text
    , postMsgReqUnfurlLinks :: Maybe Bool
    , postMsgReqUnfurlMedia :: Maybe Bool
    , postMsgReqUsername :: Maybe Text
    , postMsgReqAsUser :: Maybe Bool
    , postMsgReqIconUrl :: Maybe Text
    , postMsgReqIconEmoji :: Maybe Text
    , postMsgReqThreadTs :: Maybe Text
    , postMsgReqReplyBroadcast :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "postMsgReq") ''PostMsgReq)


-- |
--
--

instance ToForm PostMsgReq where
  toForm =
    genericToForm (formOpts "postMsgReq")


-- |
--
--

mkPostMsgReq
  :: Text
  -> Text
  -> PostMsgReq
mkPostMsgReq channel text =
  PostMsgReq
    { postMsgReqChannel = channel
    , postMsgReqText = text
    , postMsgReqParse = Nothing
    , postMsgReqLinkNames = Nothing
    , postMsgReqAttachments = Nothing
    , postMsgReqUnfurlLinks = Nothing
    , postMsgReqUnfurlMedia = Nothing
    , postMsgReqUsername = Nothing
    , postMsgReqAsUser = Nothing
    , postMsgReqIconUrl = Nothing
    , postMsgReqIconEmoji = Nothing
    , postMsgReqThreadTs = Nothing
    , postMsgReqReplyBroadcast = Nothing
    }


-- |
--
--

data PostMsgRsp =
  PostMsgRsp
    { postMsgRspTs :: String
    , postMsgRspMessage :: PostMsg
    }
  deriving (Eq, Generic, Show)

instance FromJSON PostMsgRsp where
  parseJSON = fromJsonWithOk "PostMsgRsp" $ \o ->
                PostMsgRsp <$> o .: "ts" <*> o .: "message"
