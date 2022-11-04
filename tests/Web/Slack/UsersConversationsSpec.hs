module Web.Slack.UsersConversationsSpec (spec) where

import JSONGolden
import Web.Slack.UsersConversations (UsersConversationsResponse)
import TestImport

spec :: Spec
spec = describe "User conversations method" do
  describe "Response FromJSON" do
    mapM_ (oneGoldenTest @UsersConversationsResponse) ["im_and_channels"]