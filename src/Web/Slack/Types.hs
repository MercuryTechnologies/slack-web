{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Types
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Types
  ( Color(..)
  , UserId(..)
  , SlackTimestamp(..)
  , mkSlackTimestamp
  , SlackMessageText(..)
  )
  where

-- aeson
import Data.Aeson

-- base
import Data.Monoid
import GHC.Generics (Generic)

-- errors
import Control.Error (hush)

-- http-api-data
import Web.HttpApiData

-- text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

-- time
import Data.Time.Clock
import Data.Time.Clock.POSIX

-- Ord to allow it to be a key of a Map
newtype Color = Color { unColor :: Text }
  deriving (Eq, Ord, Generic, Show, FromJSON)

-- Ord to allow it to be a key of a Map
newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, Ord, Generic, Show, FromJSON)

-- | Message text in the format returned by Slack,
-- see https://api.slack.com/docs/message-formatting
-- Consider using 'messageToHtml' for displaying.
newtype SlackMessageText = SlackMessageText { unSlackMessageText :: Text}
  deriving (Eq, Generic, Show, FromJSON)

data SlackTimestamp =
  SlackTimestamp
    { slackTimestampTs :: Text
    , slackTimestampTime :: UTCTime
    }
  deriving (Eq, Show)

instance Ord SlackTimestamp where
    compare (SlackTimestamp _ a) (SlackTimestamp _ b) = compare a b

mkSlackTimestamp :: UTCTime -> SlackTimestamp
mkSlackTimestamp utctime = SlackTimestamp (T.pack (show @Integer unixts) <> ".000000") utctime
  where unixts = floor $ toRational $ utcTimeToPOSIXSeconds utctime

instance ToHttpApiData SlackTimestamp where
  toQueryParam (SlackTimestamp contents _) = contents

instance FromJSON SlackTimestamp where
  parseJSON = withText "Slack ts" $ \contents ->
    maybe (fail "Invalid slack ts")
          (pure . SlackTimestamp contents)
          (slackTimestampToTime contents)

slackTimestampToTime :: Text -> Maybe UTCTime
slackTimestampToTime txt =
  posixSecondsToUTCTime . realToFrac @Integer . fst <$> hush (decimal txt)
