module Web.Slack.Experimental.RequestVerification
  ( SlackSigningSecret (..),
    SlackSignature (..),
    SlackRequestTimestamp (..),
    SlackVerificationFailed (..),
    validateRequest,
    validateRequest',
  )
where

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
-- See https://api.slack.com/authentication/verifying-requests-from-slack#signing_secrets_admin_page
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

data SlackVerificationFailed
  = VerificationMissingTimestamp
  | VerificationMalformedTimestamp ByteString
  | VerificationTimestampOutOfRange Int
  | VerificationMissingSignature
  | VerificationUnknownSignatureVersion ByteString
  | VerificationMalformedSignature String
  | VerificationUndecodableSignature ByteString
  | VerificationSignatureMismatch
  | VerificationCannotParse Text
  deriving stock (Show, Eq)

instance Exception SlackVerificationFailed

validateRequest ::
  (MonadIO m, FromJSON a) =>
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  m (Either SlackVerificationFailed a)
validateRequest secret sig reqTs body =
  liftIO getPOSIXTime >>= \time -> pure $ validateRequest' time secret sig reqTs body

-- | Pure version of 'validateRequest'. Probably only useful for tests.
validateRequest' ::
  FromJSON a =>
  NominalDiffTime ->
  SlackSigningSecret ->
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  Either SlackVerificationFailed a
validateRequest' now (SlackSigningSecret secret) (SlackSignature sigHeader) (SlackRequestTimestamp timestampString) body = do
  let fiveMinutes = 5 * 60
  -- timestamp must be an Int for proper basestring construction below
  timestamp <-
    maybeToRight (VerificationMalformedTimestamp timestampString) $
      fst <$> readInt timestampString
  if abs (now - fromIntegral timestamp) > fiveMinutes
    then Left $ VerificationTimestampOutOfRange timestamp
    else Right ()
  sigHeaderStripped <-
    maybeToRight (VerificationUnknownSignatureVersion sigHeader) $
      stripPrefix "v0=" sigHeader
  sigDecoded <-
    mapLeft VerificationMalformedSignature $
      B16.decode sigHeaderStripped
  sig :: HMAC SHA256 <-
    maybeToRight (VerificationUndecodableSignature sigDecoded) $
      HMAC <$> digestFromByteString sigDecoded
  let basestring = encodeUtf8 ("v0:" <> tshow timestamp <> ":") <> body
  when (hmac secret basestring /= sig) $
    Left VerificationSignatureMismatch
  mapLeft (VerificationCannotParse . pack) $ eitherDecodeStrict body
