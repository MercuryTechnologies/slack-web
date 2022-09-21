module Web.Slack.Prelude
  ( module ClassyPrelude,
    module Data.Aeson,
    module Data.Aeson.TH,
    cs,
  )
where

import ClassyPrelude hiding (link)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.TH (deriveJSON)
import Data.String.Conversions (cs)
