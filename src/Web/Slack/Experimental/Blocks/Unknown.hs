-- | Opaque storage for element kinds which this version does not implement.
-- Kept separate so callers cannot forge an unknown wrapper around a known
-- element and bypass its parser or placement restrictions.
module Web.Slack.Experimental.Blocks.Unknown (UnknownElement) where

import Control.Monad (MonadFail (..))
import Data.Aeson (Object, Value (..))
import Web.Slack.Prelude

-- | Original JSON, with a required string discriminator. Do not duplicate the
-- discriminator in another field: there must be only one source of truth.
newtype UnknownElement = UnknownElement Object
  deriving stock (Eq, Show)

instance FromJSON UnknownElement where
  parseJSON = withObject "UnknownElement" $ \obj -> do
    kind :: Text <- obj .: "type"
    case kind of
      "button" -> fail "A button must be decoded as a supported element"
      "overflow" -> fail "An overflow menu must be decoded as a supported element"
      "static_select" -> fail "A static select must be decoded as a supported element"
      "external_select" -> fail "An external select must be decoded as a supported element"
      _ -> pure $ UnknownElement obj

instance ToJSON UnknownElement where
  toJSON (UnknownElement obj) = Object obj
