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
        "<i>hello</i> <b>world</b> <a href='http://www.google.com'>google</a> <code>code</code> <pre>longer\ncode</pre>"
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
      messageToHtml "look at this:\n&gt; test *wow*" `shouldBe` "look at this:\n<blockquote>test <b>wow</b></blockquote>"
    it "parses code blocks properly" $
      messageToHtml "look at this:\n```test *wow*```" `shouldBe` "look at this:\n<pre>test *wow*</pre>"
    it "handles non-italics underscores in text well" $
      -- need to put other HTML symbols, otherwise if the parsing fails
      -- i won't find out since we default to returning the input on
      -- parsing failure
      messageToHtml "a:\n&gt;b.\n:slightly_smiling_face:" `shouldBe` "a:\n<blockquote>b.</blockquote>:slightly_smiling_face:"
    it "properly parses multiline blockquotes" $
      messageToHtml "&gt; first row\n&gt; second row\nthird row\n&gt; fourth row"
        `shouldBe`
        "<blockquote>first row<br/>second row</blockquote>third row\n<blockquote>fourth row</blockquote>"
