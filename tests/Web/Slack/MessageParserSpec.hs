{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.MessageParserSpec (spec) where

-- hspec
import Test.Hspec

-- slack-web
import Web.Slack.MessageParser

spec :: Spec
spec =
  describe "message contents parsing" $ do
    it "handles the trivial case well" $
      messageToHtml "hello" `shouldBe` "hello"
    it "converts a simple message to HTML correctly" $
      messageToHtml "_hello_ *world* <http://www.google.com|google> `code` ```longer\ncode```"
        `shouldBe`
        "<i>hello</i> <b>world</b> <a href='http://www.google.com'>google</a> <pre>code</pre> <pre>longer\ncode</pre>"
    it "degrades properly to return the input message if it's incorrect" $
      messageToHtml "link not closed <bad"
        `shouldBe`
        "link not closed <bad"
    it "creates italics sections only at word boundaries" $
      messageToHtml "false_positive" `shouldBe` "false_positive"
    it "handles bold & italics simultaneously" $
      messageToHtml "*_both_*" `shouldBe` "<b><i>both</i></b>"
    it "aborts nicely on interspersed bold & italics" $
      messageToHtml "inter *sper_ *sed_" `shouldBe` "inter *sper_ *sed_"
    it "parses blockquotes properly" $
      messageToHtml "look at this:\n> test *wow*" `shouldBe` "look at this:\n<blockquote>test <b>wow</b></blockquote>"
    it "parses code blocks properly" $
      messageToHtml "look at this:\n```test *wow*```" `shouldBe` "look at this:\n<pre>test *wow*</pre>"
