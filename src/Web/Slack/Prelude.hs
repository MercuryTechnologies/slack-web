module Web.Slack.Prelude (
  module ClassyPrelude,
  module Data.Aeson,
  module Data.Aeson.TH,
  cs,
  ToHttpApiData,
  NonEmpty (..),
  Default (..),
  Proxy (..),
) where

import ClassyPrelude hiding (link)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.TH (deriveFromJSON, deriveJSON, deriveToJSON)
import Data.Default.Class (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Web.HttpApiData (ToHttpApiData)
