module Web.Slack.Experimental.Events.TypesSpec (spec) where

import JSONGolden
import TestImport
import Web.Slack.Experimental.Events.Types

spec :: Spec
spec = describe "Types for Slack events" do
  describe "SlackWebhookEvent" do
    describe "FromJSON" do
      mapM_
        (oneGoldenTest @SlackWebhookEvent)
        [ "messageExample"
        , "messageChange"
        , "link"
        , "botMessage"
        , "joinChannel"
        , "createChannel"
        , "messageIm"
        , "slackbotIm"
        , "channel_left"
        ]
