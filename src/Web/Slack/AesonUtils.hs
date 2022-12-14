module Web.Slack.AesonUtils where

import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Types (Pair)
import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Web.FormUrlEncoded qualified as F
import Web.Slack.Prelude

-- | Checks that a record's field labels each start with the given 'prefix',
-- then uses a given 'drop (length prefix)' derivingStrategy to drop that prefix from generated JSON.
--
-- If used in a Template Haskell splice, gives a compile-time error if the prefixes don't match up.
-- Warning: This function should not be used outside of a Template Haskell splice, as it calls `error` in the case that the prefixes don't match up!
--
-- Example usage:
--
-- data PrefixedRecord = PrefixedRecord { prefixedRecordOne :: Int, prefixedRecordTwo :: Char }

-- $(deriveFromJSON (jsonDeriveWithAffix "prefixedRecord" jsonDeriveOptionsSnakeCase) ''PrefixedRecord)

jsonDeriveWithAffix :: Text -> (Int -> Options) -> Options
jsonDeriveWithAffix prefix derivingStrategy =
  originalOptions
    { fieldLabelModifier = \fieldLabel ->
        if prefix `isPrefixOf` T.pack fieldLabel
          then originalModifier fieldLabel
          else error $ "Prefixes don't match: `" <> T.unpack prefix <> "` isn't a prefix of `" <> fieldLabel <> "`. Search for jsonDeriveWithAffix to learn more."
    }
  where
    originalOptions = derivingStrategy $ T.length prefix
    originalModifier = fieldLabelModifier originalOptions

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (c : chars) = Char.toLower c : chars

jsonDeriveOptionsSnakeCase :: Int -> Options
jsonDeriveOptionsSnakeCase n =
  defaultOptions
    { fieldLabelModifier = camelToSnake . lowerFirst . drop n
    , omitNothingFields = True
    , constructorTagModifier = camelToSnake . lowerFirst . drop n
    }

-- | Create a 'Value' from a list of name\/value @Maybe Pair@'s.
-- For 'Nothing', instead of outputting @null@, that field will not be output at all.
-- If duplicate keys arise, later keys and their associated values win.
--
-- Example:
--
-- @
-- objectOptional
--   [ "always" .=! 1
--   , "just" .=? Just 2
--   , "nothing" .=? Nothing
--   ]
-- @
--
-- will result in the JSON
--
-- @
-- {
--   "always": 1,
--   "just": 2
-- }
-- @
--
-- The field @nothing@ is ommited because it was 'Nothing'.
objectOptional :: [Maybe Pair] -> Value
objectOptional = J.object . catMaybes

-- | Encode a value for 'objectOptional'
(.=!) :: ToJSON v => Key -> v -> Maybe Pair
key .=! val = Just (key .= val)

infixr 8 .=!

-- | Encode a Maybe value for 'objectOptional'
(.=?) :: ToJSON v => Key -> Maybe v -> Maybe Pair
key .=? mVal = fmap (key .=) mVal

infixr 8 .=?

-- | Conditionally encode a value for 'objectOptional'
(?.>) :: Bool -> Pair -> Maybe Pair
True ?.> pair = Just pair
False ?.> _ = Nothing

infixr 7 ?.>

-- | Conditionally express a pair in a JSON series
thenPair :: Bool -> J.Series -> J.Series
thenPair True s = s
thenPair False _ = mempty

infixr 7 `thenPair`

snakeCaseOptions :: Options
snakeCaseOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , constructorTagModifier = camelTo2 '_'
    }

snakeCaseFormOptions :: F.FormOptions
snakeCaseFormOptions =
  F.defaultFormOptions
    { F.fieldLabelModifier = camelTo2 '_'
    }

newtype UnixTimestamp = UnixTimestamp {unUnixTimestamp :: UTCTime}
  deriving newtype (Show, Eq)

instance FromJSON UnixTimestamp where
  parseJSON a = UnixTimestamp . posixSecondsToUTCTime <$> parseJSON a

instance ToJSON UnixTimestamp where
  toJSON (UnixTimestamp a) = toJSON (utcTimeToPOSIXSeconds a)
