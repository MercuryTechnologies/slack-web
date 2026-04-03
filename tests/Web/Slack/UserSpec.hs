module Web.Slack.UserSpec (spec) where

import JSONGolden
import TestImport
import Web.Slack.User (ListRsp)

-- Fixture modeled on the example response at
-- https://api.slack.com/methods/users.list#examples
spec :: Spec
spec = describe "User methods" do
  describe "users.list" do
    describe "Response FromJSON" do
      mapM_ (oneGoldenTestDecode @ListRsp) ["with_bots"]
