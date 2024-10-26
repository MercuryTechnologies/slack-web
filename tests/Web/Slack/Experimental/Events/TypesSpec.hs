module Web.Slack.Experimental.Events.TypesSpec (spec) where

import JSONGolden
import TestImport
import Web.Slack.Experimental.Events.Types

spec :: Spec
spec = describe "Types for Slack events" do
  describe "SlackWebhookEvent" do
    describe "FromJSON" do
      mapM_
        (oneGoldenTestDecode @SlackWebhookEvent)
        [ "messageExample"
        , "messageChange"
        , "message_rich_text"
        , "message_file_share"
        , "message_file_share_slack_connect"
        , "link"
        , "botMessage"
        , "joinChannel"
        , "createChannel"
        , "messageIm"
        , "slackbotIm"
        , "channel_left"
        , "share_without_message"
        , "share_with_message"
        , "circleci"
        , -- https://slack.com/help/articles/206819278-Send-emails-to-Slack
          "email_message"
        , "message_subtype_bot_message"
        , "forwarded_message"
        , "github_notification"
        , "github_notification_ts_string"
        , "non_spec_attachment"
        ]
