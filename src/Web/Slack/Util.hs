module Web.Slack.Util
  ( formOpts
  , jsonOpts
  )
  where

-- aeson
import Data.Aeson.TH

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
  let
    go res [] = res
    go [] (x:xs) = go [toLower x] xs
    go res (x:xs)
      | isUpper x = go (toLower x : '_' : res) xs
      | otherwise = go (x : res) xs

  in
    reverse . go []
