----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Types
-- Description:
module Web.Slack.Types (
  Color (..),
  UserId (..),
  ConversationId (..),
  TeamId (..),
  Cursor (..),
  SlackTimestamp (..),
  mkSlackTimestamp,
  timestampFromText,
  SlackMessageText (..),
) where

import Control.Monad (MonadFail (..))
import Data.Aeson
import Data.Text qualified as T
import Data.Text.Read (rational)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Web.HttpApiData
import Web.Slack.Pager.Types
import Web.Slack.Prelude

-- Ord to allow it to be a key of a Map
newtype Color = Color {unColor :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON)

-- Ord to allow it to be a key of a Map
newtype UserId = UserId {unUserId :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, ToHttpApiData)

-- | Common identifier for every type of 'Conversation'.
--   Unique to the team which the conversation belongs to.
-- Ord to allow it to be a key of a Map
newtype ConversationId = ConversationId {unConversationId :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, ToHttpApiData)

-- Ord to allow it to be a key of a Map
newtype TeamId = TeamId {unTeamId :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, ToHttpApiData)

-- | Message text in the format returned by Slack,
-- see https://api.slack.com/docs/message-formatting
-- Consider using 'messageToHtml' for displaying.
newtype SlackMessageText = SlackMessageText {unSlackMessageText :: Text}
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON)

data SlackTimestamp = SlackTimestamp
  { slackTimestampTs :: Text
  , slackTimestampTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance NFData SlackTimestamp

instance Ord SlackTimestamp where
  compare (SlackTimestamp _ a) (SlackTimestamp _ b) = compare a b

-- | Convert timestamp texts e.g. "1595719220.011100" into 'SlackTimestamp'
timestampFromText :: Text -> Either String SlackTimestamp
timestampFromText t = f =<< rational t
  where
    f (posixTime, "") =
      Right . SlackTimestamp t $ posixSecondsToUTCTime posixTime
    f (_, _left) = Left "Unexpected text left after timestamp"

mkSlackTimestamp :: UTCTime -> SlackTimestamp
mkSlackTimestamp utctime = SlackTimestamp (take6DigitsAfterPoint $ T.pack (show unixts)) utctime
  where
    unixts = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utctime
    take6DigitsAfterPoint = uncurry (<>) . second (T.take 7) . T.break (== '.')

instance ToHttpApiData SlackTimestamp where
  toQueryParam (SlackTimestamp contents _) = contents

instance FromJSON SlackTimestamp where
  parseJSON =
    withText "Slack ts"
      $ either (fail . ("Invalid Slack ts: " ++)) pure
      . timestampFromText

instance ToJSON SlackTimestamp where
  toJSON = String . slackTimestampTs
