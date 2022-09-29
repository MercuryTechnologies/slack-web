{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.MessageParserSpec (spec) where

-- hspec

-- slack-web

-- text
import Data.Text (Text)
import Test.Hspec
import Web.Slack.MessageParser
import Web.Slack.Types

testGetUserDesc :: UserId -> Text
testGetUserDesc (UserId "USER1") = "user_one"
testGetUserDesc x = unUserId x

testHtmlRenderers :: HtmlRenderers
testHtmlRenderers =
  HtmlRenderers
    { emoticonRenderer = \x -> ":>" <> x <> "<:"
    }

msgToHtml :: Text -> Text
msgToHtml = messageToHtml testHtmlRenderers testGetUserDesc . SlackMessageText

spec :: Spec
spec =
  describe "message contents parsing" $ do
    it "handles the trivial case well" $
      msgToHtml "hello" `shouldBe` "hello"
    it "converts a simple message to HTML correctly" $
      msgToHtml "_hello_ *world* <http://www.google.com|google> `code` ```longer\ncode```"
        `shouldBe` "<i>hello</i> <b>world</b> <a href='http://www.google.com'>google</a> <code>code</code> <pre>longer\ncode</pre>"
    it "degrades properly to return the input message if it's incorrect" $
      msgToHtml "link not closed <bad"
        `shouldBe` "link not closed <bad"
    it "creates italics sections only at word boundaries" $
      msgToHtml "false_positive" `shouldBe` "false_positive"
    it "handles bold, strikethrough & italics simultaneously" $
      msgToHtml "*_~both~_*" `shouldBe` "<b><i><strike>both</strike></i></b>"
    it "aborts nicely on interspersed bold & italics" $
      msgToHtml "inter *sper_ *sed_" `shouldBe` "inter *sper_ *sed_"
    it "parses blockquotes properly" $
      msgToHtml "look at this:\n&gt; test *wow*" `shouldBe` "look at this:<br/><blockquote>test <b>wow</b></blockquote>"
    it "parses code blocks properly" $
      msgToHtml "look at this:\n```test *wow*```" `shouldBe` "look at this:<br/><pre>test *wow*</pre>"
    it "handles non-italics underscores in text well" $
      -- need to put other HTML symbols, otherwise if the parsing fails
      -- i won't find out since we default to returning the input on
      -- parsing failure
      msgToHtml "a:\n&gt;b.\n:slightly_smiling_face:" `shouldBe` "a:<br/><blockquote>b.</blockquote>:>slightly_smiling_face<:"
    it "properly parses multiline blockquotes" $
      msgToHtml "&gt; first row\n&gt; second row\nthird row\n&gt; fourth row"
        `shouldBe` "<blockquote>first row<br/>second row</blockquote>third row<br/><blockquote>fourth row</blockquote>"
    it "converts usernames" $
      msgToHtml "<@USER1> should be converted, <@USER1|default> stay default"
        `shouldBe` "@user_one should be converted, @default stay default"
    it "converts carriage returns" $
      msgToHtml "a\nb" `shouldBe` "a<br/>b"
    it "handles full stops as punctuation" $
      msgToHtml "*b*." `shouldBe` "<b>b</b>."
