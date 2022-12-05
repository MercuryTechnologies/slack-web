module Web.Slack.ChatSpec (spec) where

import JSONGolden
import TestImport
import Web.Slack.Chat (UpdateRsp)

spec :: Spec
spec = describe "Chat methods" do
  describe "chat.update" do
    describe "Response FromJSON" do
      mapM_ (oneGoldenTestDecode @UpdateRsp) ["sample", "actual"]
