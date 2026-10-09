module Web.Slack.CommonSpec (spec) where

import Control.Exception (ErrorCall (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Servant.Client qualified as Servant
import TestImport
import Web.Slack.Common

spec :: Spec
spec = describe "displayException" do
  it "displays a Slack API error without empty metadata" do
    let err = ResponseSlackError "invalid_auth" mempty
    displayException err `shouldBe` "Slack API response error: invalid_auth"
    displayException (SlackError err) `shouldBe` "Slack API response error: invalid_auth"

  it "includes response metadata as JSON, preserving Unicode" do
    let err = ResponseSlackError "invalid_arguments" (KeyMap.fromList [("messages", toJSON (["Invalid value: café"] :: [Text]))])
    displayException err `shouldBe` "Slack API response error: invalid_arguments: {\"messages\":[\"Invalid value: café\"]}"
    displayException (SlackError err) `shouldBe` "Slack API response error: invalid_arguments: {\"messages\":[\"Invalid value: café\"]}"

  it "prefixes the underlying Servant error" do
    let err = ServantError (Servant.ConnectionError (toException (ErrorCall "connection refused")))
    displayException err `shouldBe` "Servant error: ConnectionError connection refused"
