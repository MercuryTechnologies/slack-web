{-# LANGUAGE OverloadedStrings #-}

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
  , fromJsonWithOk
  )
  where

-- aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- base
import Data.Char

-- http-api-data
import Web.FormUrlEncoded (FormOptions(FormOptions))

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

fromJsonWithOk :: String -> (Text -> a) -> (Object -> Parser a) -> Value -> Parser a
fromJsonWithOk objName errorCtor okReader = withObject objName $ \o -> do
      ok <- o .: "ok"
      if ok
         then okReader o
         else errorCtor <$> o .: "error"
