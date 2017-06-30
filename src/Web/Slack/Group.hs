{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Group
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Group
  ( Group(..)
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Group =
  Group
    { groupId :: Text
    , groupName :: Text
    , groupIsMpim :: Bool
    , groupCreated :: POSIXTime
    , groupCreator :: Text
    , groupIsArchived :: Bool
    , groupMembers :: [Text]
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "group") ''Group)

data ListRsp =
  ListRsp
    { listRspGroups :: [Group]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
