{-# LANGUAGE TemplateHaskell #-}

-- | Type definitions for the Slack files APIs.
-- See <https://api.slack.com/messaging/files>.
--
-- @since 1.6.0.0
module Web.Slack.Files.Types where

import Control.Monad.Fail (MonadFail (..))
import Data.Aeson ((.!=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Web.Slack.AesonUtils (UnixTimestamp, snakeCaseOptions)
import Web.Slack.Prelude

-- | ID for a file, which looks something like @F2147483862@.
newtype FileId = FileId {unFileId :: Text}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

data FileMode
  = Hosted
  | External
  | Snippet
  | Post
  | FileAccess
  | -- | <https://slack.com/help/articles/206819278-Send-emails-to-Slack>
    --
    -- @since 1.6.1.0
    Email
  | -- | Other file modes.
    --
    --   @since 1.6.1.0
    Other Text
  deriving stock (Show, Eq, Generic)

instance FromJSON FileMode where
  parseJSON = A.withText "FileMode" \case
    "hosted" -> pure Hosted
    "external" -> pure External
    "snippet" -> pure Snippet
    "post" -> pure Post
    "file_access" -> pure FileAccess
    "email" -> pure Email
    other -> pure . Other $ other

instance ToJSON FileMode where
  toJSON =
    A.String . \case
      Hosted -> "hosted"
      External -> "external"
      Snippet -> "snippet"
      Post -> "post"
      FileAccess -> "file_access"
      Email -> "email"
      Other s -> s

-- | <https://api.slack.com/types/file>
data FileObjectVisible = FileObjectVisible
  { id :: FileId
  , created :: UnixTimestamp
  , name :: Text
  , title :: Text
  , mimetype :: Text
  , urlPrivate :: Text
  , isExternal :: Bool
  , size :: Int
  , mode :: FileMode
  }
  deriving stock (Show, Eq)

$(deriveJSON snakeCaseOptions ''FileObjectVisible)

data FileObject
  = -- | File object is visible
    VisibleFileObject FileObjectVisible
  | -- | File object is in a shared channel so @files.info@ must be invoked to
    -- get any further details. See
    -- <https://api.slack.com/types/file#slack_connect_files> for more details.
    CheckFileInfo FileId
  deriving stock (Show, Eq)

instance FromJSON FileObject where
  parseJSON = withObject "FileObject" $ \obj -> do
    -- "visible" is undocumented, thanks Slack!
    ty :: Text <- obj .:? "file_access" .!= "visible"
    case ty of
      "visible" -> VisibleFileObject <$> parseJSON (A.Object obj)
      "check_file_info" -> CheckFileInfo <$> obj .: "id"
      _ -> fail $ "unknown file_access type " <> unpack ty

instance ToJSON FileObject where
  toJSON (VisibleFileObject obj) = case toJSON obj of
    A.Object o -> A.Object $ o <> KM.fromList [("file_access", "visible")]
    _ -> error "impossible"
  toJSON (CheckFileInfo fid) = A.object [("file_access", "check_file_info"), ("id", toJSON fid)]
