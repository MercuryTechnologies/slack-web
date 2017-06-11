{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)


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
