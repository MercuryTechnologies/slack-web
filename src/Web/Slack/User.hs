{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.User
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.User
  ( User(..)
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Common
import Web.Slack.Util

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data User =
  User
    { userId :: UserId
    , userName :: Text
    , userDeleted :: Bool
    , userColor :: Maybe Color
    , userIsAdmin :: Maybe Bool
    , userIsOwner :: Maybe Bool
    , userIsPrimaryOwner :: Maybe Bool
    , userIsRestricted :: Maybe Bool
    , userIsUltraRestricted :: Maybe Bool
    , userUpdated :: POSIXTime
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "user") ''User)

data ListRsp =
  ListRsp
    { listRspOk :: Bool
    , listRspMembers :: [User]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
