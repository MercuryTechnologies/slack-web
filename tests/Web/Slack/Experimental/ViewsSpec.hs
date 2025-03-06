{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Slack.Experimental.ViewsSpec where

import JSONGolden
import TestImport
import Web.Slack.Experimental.Views

spec :: Spec
spec = describe "Slack views" do
  describe "SlackView ModalView" do
    describe "FromJSON" do
      mapM_
        (oneGoldenTestDecode @(SlackView ModalView))
        -- Taken from: https://api.slack.com/reference/surfaces/views#modal__modal-view-example
        ["modalView"]

  describe "SlackView HomeTabView" do
    mapM_
      (oneGoldenTestDecode @(SlackView HomeTabView))
      -- https://api.slack.com/reference/surfaces/views#home-example
      ["homeTabView"]

  describe "views.publish response" do
    oneGoldenTestDecode @PublishResp "views.publish example"
