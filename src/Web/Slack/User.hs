{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | @users.*@ methods in the Slack API
module Web.Slack.User
  ( Profile (..),
    User (..),
    ListReq (..),
    ListRsp (..),
    Email (..),
    UserRsp (..),
  )
where

import Data.Time.Clock.POSIX
import Web.FormUrlEncoded
import Web.HttpApiData
import Web.Slack.Common
import Web.Slack.Pager.Types (PagedRequest (..), PagedResponse (..), ResponseMetadata)
import Web.Slack.Prelude
import Web.Slack.Util

-- | See <https://api.slack.com/types/user>
data Profile = Profile
  { profileAvatarHash :: Maybe Text
  , profileStatusText :: Maybe Text
  , profileStatusEmoji :: Maybe Text
  , profileRealName :: Maybe Text
  , profileDisplayName :: Maybe Text
  , profileRealNameNormalized :: Maybe Text
  , profileDisplayNameNormalized :: Maybe Text
  , profileEmail :: Maybe Text
  , profileImage_24 :: Text
  , profileImage_32 :: Text
  , profileImage_48 :: Text
  , profileImage_72 :: Text
  , profileImage_192 :: Text
  , profileImage_512 :: Text
  , profileTeam :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "profile") ''Profile)

data User = User
  { userId :: UserId
  , userName :: Text
  , userDeleted :: Bool
  , userColor :: Maybe Color
  , userProfile :: Maybe Profile
  , userIsAdmin :: Maybe Bool
  , userIsOwner :: Maybe Bool
  , userIsPrimaryOwner :: Maybe Bool
  , userIsRestricted :: Maybe Bool
  , userIsUltraRestricted :: Maybe Bool
  , userUpdated :: POSIXTime
  }
  deriving stock (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "user") ''User)

-- | @users.list@ request. See <https://api.slack.com/methods/users.list#args>
--
-- @since 1.6.0.0
data ListReq = ListReq
  { listReqCursor :: Maybe Cursor
  , listReqLimit :: Maybe Int
  , listReqTeamId :: Maybe TeamId
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Default)

instance ToForm ListReq where
  toForm (ListReq {listReqCursor, listReqLimit, listReqTeamId}) =
    toQueryParamIfJust "cursor" listReqCursor
      <> toQueryParamIfJust "limit" listReqLimit
      <> toQueryParamIfJust "team_id" listReqTeamId

instance PagedRequest ListReq where
  setCursor c r = r {listReqCursor = c}

-- | Response to <https://api.slack.com/methods/users.list>
data ListRsp = ListRsp
  { listRspMembers :: [User]
  , listRspResponseMetadata :: Maybe ResponseMetadata
  }
  deriving stock (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)

instance PagedResponse ListRsp where
  type ResponseObject ListRsp = User
  getResponseData = listRspMembers
  getResponseMetadata = listRspResponseMetadata

data UserRsp = UserRsp
  { userRspUser :: User
  }
  deriving stock (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "UserRsp") ''UserRsp)

newtype Email = Email Text deriving stock (Eq, Generic, Show)

instance ToForm Email where
  toForm (Email txt) = [("email", toQueryParam txt)]
