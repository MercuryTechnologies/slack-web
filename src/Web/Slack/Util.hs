----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Util
-- Description:
module Web.Slack.Util (
  formOpts,
  jsonOpts,
  toQueryParamIfJust,
) where

-- FIXME: Web.Slack.Prelude

-- aeson
import Data.Aeson.TH
import Data.Aeson.Types
-- base
import Data.Char
import Data.Maybe (maybeToList)
-- http-api-data

-- text
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (fromList)
import Web.FormUrlEncoded (Form, FormOptions (FormOptions))
import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Prelude

formOpts ::
  Text ->
  FormOptions
formOpts prefix =
  FormOptions (modifyLabel prefix)

jsonOpts ::
  Text ->
  Options
jsonOpts prefix =
  defaultOptions
    { fieldLabelModifier = modifyLabel prefix
    }

modifyLabel ::
  Text ->
  String ->
  String
modifyLabel prefix =
  fmap toLower
    . addUnderscores
    . drop (Text.length prefix)

addUnderscores ::
  String ->
  String
addUnderscores =
  camelTo2 '_'

toQueryParamIfJust :: (ToHttpApiData a) => Text -> Maybe a -> Form
toQueryParamIfJust key =
  fromList . maybeToList . fmap (\justVal -> (key, toQueryParam justVal))
