{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.MessageParserSpec (spec) where

-- hspec
import Test.Hspec

-- slack-web
import Web.Slack.MessageParser

spec :: Spec
spec = do
  describe "message contents parsing" $ do
    it "converts a simple message to HTML correctly" $
      messageToHtml "hel_lo *world_* <http://www.google.com|google> `code` ```longer\ncode```"
        `shouldBe`
        "hel<i>lo </i><b><i>world</i></b> <a href='http://www.google.com'>google</a> <pre>code</pre> <pre>longer\ncode</pre>"
    it "degrades properly to return the input message if it's incorrect" $
      messageToHtml "link not closed <bad"
        `shouldBe`
        "link not closed <bad"
