module Web.Slack.Experimental.RequestVerification (
  SlackSigningSecret (..),
  SlackSignature (..),
  SlackRequestTimestamp (..),
  SlackVerificationFailed,
  SlackVerificationFailed' (..),
  validateRequest,
  validateRequest',
  validateRequestRaw,
  validateRequestRaw',
  decodeRequestJSON,
) where

import Crypto.Hash (SHA256, digestFromByteString)
import Crypto.MAC.HMAC
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 (readInt)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.HttpApiData (FromHttpApiData (..))
import Web.Slack.Prelude

-- | Slack generated Signing Secret placed into configuration.
-- See <https://api.slack.com/authentication/verifying-requests-from-slack#signing_secrets_admin_page>
newtype SlackSigningSecret
  = SlackSigningSecret ByteString
  deriving stock (Eq)

instance Show SlackSigningSecret where
  show _ = "<SlackSigningSecret>"

newtype SlackSignature = SlackSignature ByteString
  deriving newtype (Eq, Show)

newtype SlackRequestTimestamp = SlackRequestTimestamp ByteString
  deriving newtype (Eq, Show)

instance FromHttpApiData SlackRequestTimestamp where
  parseQueryParam _ = error "SlackRequestTimestamp should not be in a query param"
  parseUrlPiece _ = error "SlackRequestTimestamp should not be in a url piece"
  parseHeader = Right . SlackRequestTimestamp

instance FromHttpApiData SlackSignature where
  parseQueryParam _ = error "SlackSignature should not be in a query param"
  parseUrlPiece _ = error "SlackSignature should not be in a url piece"
  parseHeader = Right . SlackSignature

type SlackVerificationFailed = SlackVerificationFailed' Text

data SlackVerificationFailed' parseError
  = VerificationMissingTimestamp
  | VerificationMalformedTimestamp ByteString
  | VerificationTimestampOutOfRange Int
  | VerificationMissingSignature
  | VerificationUnknownSignatureVersion ByteString
  | VerificationMalformedSignature String
  | VerificationUndecodableSignature ByteString
  | VerificationSignatureMismatch
  | VerificationCannotParse parseError
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

type role SlackVerificationFailed' representational

-- | Decodes the JSON request body as plain JSON (as would be seen in a
-- 'SlackWebhookEvent').
--
-- @since 2.3.0.0
decodeRequestJSON :: (FromJSON body) => ByteString -> Either Text body
decodeRequestJSON = mapLeft pack . eitherDecodeStrict

-- | Validates that a Slack request is signed appropriately to prove it
-- originated from Slack, then decodes it as JSON, in the spirit of "Parse,
-- don't validate".
--
-- See: <https://api.slack.com/authentication/verifying-requests-from-slack>
validateRequest ::
  (MonadIO m, FromJSON body) =>
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  m (Either SlackVerificationFailed body)
validateRequest = validateRequestRaw decodeRequestJSON

-- | Pure version of 'validateRequest'. Probably only useful for tests.
validateRequest' ::
  (FromJSON body) =>
  NominalDiffTime ->
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  Either SlackVerificationFailed body
validateRequest' = validateRequestRaw' decodeRequestJSON

-- | Validates that a Slack request is signed appropriately to prove it
-- originated from Slack, then decodes it, in the spirit of "Parse, don't
-- validate".
--
-- This function is necessary for the interactive webhooks that are
-- x-form-urlencoded with a @payload@ field. For more info on those, see
-- <https://api.slack.com/interactivity/handling#payloads>
--
-- See: <https://api.slack.com/authentication/verifying-requests-from-slack>
--
-- @since 2.3.0.0
validateRequestRaw ::
  (MonadIO m) =>
  (ByteString -> Either err body) ->
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  m (Either (SlackVerificationFailed' err) body)
validateRequestRaw decoder secret sig reqTs body =
  liftIO getPOSIXTime >>= \time -> pure $ validateRequestRaw' decoder time secret sig reqTs body

-- | Pure version of 'validateRequestRaw'. Probably only useful for tests.
--
-- @since 2.3.0.0
validateRequestRaw' ::
  (ByteString -> Either err body) ->
  NominalDiffTime ->
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  Either (SlackVerificationFailed' err) body
validateRequestRaw' decoder now (SlackSigningSecret secret) (SlackSignature sigHeader) (SlackRequestTimestamp timestampString) body = do
  let fiveMinutes = 5 * 60
  -- timestamp must be an Int for proper basestring construction below
  timestamp <-
    maybeToRight (VerificationMalformedTimestamp timestampString)
      $ fst
      <$> readInt timestampString
  if abs (now - fromIntegral timestamp) > fiveMinutes
    then Left $ VerificationTimestampOutOfRange timestamp
    else Right ()
  sigHeaderStripped <-
    maybeToRight (VerificationUnknownSignatureVersion sigHeader)
      $ stripPrefix "v0=" sigHeader
  sigDecoded <-
    mapLeft VerificationMalformedSignature
      $ B16.decode sigHeaderStripped
  sig :: HMAC SHA256 <-
    maybeToRight (VerificationUndecodableSignature sigDecoded)
      $ HMAC
      <$> digestFromByteString sigDecoded
  let basestring = encodeUtf8 ("v0:" <> tshow timestamp <> ":") <> body
  when (hmac secret basestring /= sig)
    $ Left VerificationSignatureMismatch
  mapLeft VerificationCannotParse $ decoder body
