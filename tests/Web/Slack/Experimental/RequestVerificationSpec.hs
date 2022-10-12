module Web.Slack.Experimental.RequestVerificationSpec where

import TestImport
import Data.Time.Clock.POSIX
import Web.Slack.Experimental.RequestVerification
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap

spec :: Spec
spec = do
  let timestamp = SlackRequestTimestamp "1665615887" -- Oct 12 2022 23:04:47 GMT+0000
  let sig = SlackSignature "v0=f7f0a2f48ae5fe2c923813af0a7c7636396fea6e03edb3d73580a99cd1913575"
  let slackSigningSecret = SlackSigningSecret "SIGNING_SECRET"
      -- matches the test timestamp within one minute
      goodUtcTime = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2022 10 12) (23 * 60 * 60 + 5 * 60)
      -- back to the future
      someoneGotATimeMachineUtcTime = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2020 1 1) 0
      oldUtcTime = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 2025 10 12) (23 * 60 * 60 + 5 * 60)

      -- the simplest possible JSON; this thing just parses it straight through
      -- anyway
      requestBody = "{}"

  describe "Slack Webhook" $ do
    it "Correctly validates webhooks" $ do
      validateRequest' goodUtcTime slackSigningSecret sig timestamp requestBody
        `shouldBe` (Right $ Object KeyMap.empty)

    it "Handles incorrect timestamp" $ do
      validateRequest' @Value goodUtcTime slackSigningSecret sig (SlackRequestTimestamp "bad") requestBody
        `shouldBe` Left (VerificationMalformedTimestamp "bad")

    it "Handles invalid signature" $ do
      validateRequest' @Value goodUtcTime slackSigningSecret (SlackSignature "the_v0_prefix_missing") timestamp requestBody
        `shouldBe` Left (VerificationUnknownSignatureVersion "the_v0_prefix_missing")

    it "Handles signature mismatch" $ do
      let badSignature = SlackSignature "v0=bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0bad0"
      validateRequest' @Value goodUtcTime slackSigningSecret badSignature timestamp requestBody
        `shouldBe` Left VerificationSignatureMismatch

    it "Handles timestamp in the future" $ do
      -- it's "in 2020" and someone gave us a 2022 timestamp
      validateRequest' @Value someoneGotATimeMachineUtcTime slackSigningSecret sig timestamp requestBody
        `shouldBe` Left (VerificationTimestampOutOfRange 1665615887)

    it "Handles timestamp in the past" $ do
      -- it's "in 2025" and someone gave us a 2022 timestamp
      validateRequest' @Value oldUtcTime slackSigningSecret sig timestamp requestBody
        `shouldBe` (Left (VerificationTimestampOutOfRange 1665615887))
