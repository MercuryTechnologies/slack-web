----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Util
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Util
  ( formOpts
  , jsonOpts
  , toQueryParamIfJust
  )
  where

-- aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- base
import Data.Char
import Data.Maybe (maybeToList)
import GHC.Exts (fromList)

-- http-api-data
import Web.HttpApiData (toQueryParam, ToHttpApiData)
import Web.FormUrlEncoded (Form, FormOptions(FormOptions))

-- text
import Data.Text (Text)
import qualified Data.Text as Text


-- |
--
--

formOpts
  :: Text
  -> FormOptions
formOpts prefix =
  FormOptions (modifyLabel prefix)


-- |
--
--

jsonOpts
  :: Text
  -> Options
jsonOpts prefix =
  defaultOptions
    { fieldLabelModifier = modifyLabel prefix
    }


-- |
--
--

modifyLabel
  :: Text
  -> String
  -> String
modifyLabel prefix =
  fmap toLower
    . addUnderscores
    . drop (Text.length prefix)


-- |
--
--

addUnderscores
  :: String
  -> String
addUnderscores =
  camelTo2 '_'


toQueryParamIfJust :: ToHttpApiData a => Text -> Maybe a -> Form
toQueryParamIfJust key =
  fromList . maybeToList . fmap (\justVal -> (key, toQueryParam justVal))
