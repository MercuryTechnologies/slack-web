module Web.Slack.Experimental.Blocks.TypesSpec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Data.StringVariants.NonEmptyText.Internal (pattern NonEmptyText)
import Refined.Unsafe (reallyUnsafeRefine)
import TestImport
import Web.Slack.Common (ConversationId (..))
import Web.Slack.Experimental.Blocks.Types

jsonRoundtrips :: (Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Spec
jsonRoundtrips a = do
  it "can decode its own json encoding" do
    (Aeson.fromJSON . Aeson.toJSON) a `shouldBe` Aeson.Success a

spec :: Spec
spec = do
  describe "unknown component diagnostics" do
    it "reports the rejected text object type" do
      Aeson.eitherDecode @SlackTextObject "{\"type\":\"unexpected_text\"}"
        `shouldBe` Left "Error in $: Unknown SlackTextObject type \"unexpected_text\", must be one of ['plain_text', 'mrkdwn']"

    it "reports the rejected content type at its nested JSON path" do
      Aeson.eitherDecode @SlackBlock "{\"type\":\"context\",\"elements\":[{\"type\":\"unexpected_content\"}]}"
        `shouldBe` Left "Error in $.elements: Unknown SlackContent type \"unexpected_content\", must be one of ['mrkdwn', 'image']"

    it "reports the rejected action component type at its nested JSON path" do
      Aeson.eitherDecode @SlackBlock "{\"type\":\"actions\",\"elements\":[{\"type\":\"static_select\",\"action_id\":\"select\"}]}"
        `shouldBe` Left "Error in $.elements[0]: Unknown SlackActionComponent type \"static_select\", must be one of ['button']"

    it "reports the rejected accessory type before requiring action fields" do
      Aeson.eitherDecode @SlackAccessory "{\"type\":\"image\",\"image_url\":\"https://example.com/image.png\",\"alt_text\":\"example\"}"
        `shouldBe` Left "Error in $: Unknown SlackAccessory type \"image\", must be one of ['button']"

    it "reports the rejected accessory type even when action fields are present" do
      Aeson.eitherDecode @SlackAccessory "{\"type\":\"static_select\",\"action_id\":\"select\"}"
        `shouldBe` Left "Error in $: Unknown SlackAccessory type \"static_select\", must be one of ['button']"

  describe "incoming Block Kit components" do
    it "parses a message mention without a URL" do
      Aeson.eitherDecode @RichItem "{\"type\":\"message_mention\",\"channel_id\":\"C123ABC456\",\"message_ts\":\"1720710212.123456\"}"
        `shouldBe` Right (RichItemMessageMention (RichMessageMention (ConversationId "C123ABC456") "1720710212.123456" Nothing Nothing))

    describe "rich-text list entries" do
      let listWith entry =
            object
              [ "type" .= ("rich_text_list" :: Text)
              , "style" .= ("bullet" :: Text)
              , "elements" .= [entry]
              ]
          decodeList = parseEither (parseJSON @RichTextSectionItem) . listWith

      for_ ["rich_text_list", "rich_text_quote", "rich_text_preformatted", "text", "future_container"] \(kind :: Text) ->
        it ("rejects a " <> unpack kind <> " as a list entry") do
          decodeList
            (object ["type" .= kind, "elements" .= ([] :: [Aeson.Value]), "style" .= ("bullet" :: Text), "text" .= ("inline" :: Text)])
            `shouldSatisfy` isLeft

      it "requires the section type tag" do
        decodeList (object ["elements" .= ([] :: [Aeson.Value])]) `shouldSatisfy` isLeft

      it "requires the section contents" do
        decodeList (object ["type" .= ("rich_text_section" :: Text)]) `shouldSatisfy` isLeft

      it "preserves unknown inline items inside a section" do
        let inlineItem = object ["type" .= ("future_inline" :: Text), "text" .= ("preserved" :: Text)]
        decodeList (object ["type" .= ("rich_text_section" :: Text), "elements" .= [inlineItem]])
          `shouldBe` Right (RichTextSectionItemList [RichTextSection [RichItemOther "future_inline" inlineItem]])

  let
    aSlackAccessory = SlackButtonAccessory aSlackAction
    aSlackAction = SlackAction
      do SlackActionId $ NonEmptyText "action-id"
      do aSlackButton
    aSlackActionList =
      SlackActionList
        . reallyUnsafeRefine
        $ [ aSlackAction
          , SlackAction
              do SlackActionId $ NonEmptyText "another-action-id"
              do
                SlackButton
                  do SlackButtonText $ NonEmptyText "another-button-text"
                  do Nothing
                  do Nothing
                  do Nothing
                  do Nothing
          ]
    aSlackBlockSection = SlackBlockSection aSlackSection
    aSlackBlockImage = SlackBlockImage aSlackImage
    aSlackBlockContext = SlackBlockContext aSlackContext
    aSlackBlockActions = SlackBlockActions
      do Just $ NonEmptyText "block-actions"
      do aSlackActionList
    aSlackBlockHeader = SlackBlockHeader $ SlackPlainTextOnly "block-header"
    aSlackButton =
      SlackButton
        { slackButtonText = SlackButtonText (NonEmptyText "button-text")
        , slackButtonUrl = Just (NonEmptyText "button-url")
        , slackButtonValue = Just (NonEmptyText "button-value")
        , slackButtonStyle = Just SlackStylePrimary
        , slackButtonConfirm = Just aSlackConfirmObject
        }
    aSlackConfirmObject =
      SlackConfirmObject
        { slackConfirmTitle = SlackPlainTextOnly "button-confirm-title"
        , slackConfirmText = SlackPlainText "button-confirm-text"
        , slackConfirmConfirm = SlackPlainTextOnly "button-confirm-confirm"
        , slackConfirmDeny = SlackPlainTextOnly "button-confirm-deny"
        , slackConfirmStyle = Just SlackStyleDanger
        }
    aSlackContentText = SlackContentText "content-text"
    aSlackContentImage = SlackContentImage aSlackImage
    aSlackContext = SlackContext [aSlackContentText, aSlackContentImage]
    aSlackImage =
      SlackImage
        { slackImageTitle = Just "image-title"
        , slackImageAltText = "image-alt-text"
        , slackImageUrl = "image-url"
        }
    aSlackMessage =
      SlackMessage
        [ aSlackBlockSection
        , aSlackBlockImage
        , aSlackBlockContext
        , aSlackBlockActions
        , aSlackBlockHeader
        -- not tested: SlackBlockRichText
        ]
    aSlackPlainTextOnly = SlackPlainTextOnly "plain-text-only"
    aSlackPlainText = SlackPlainText "plain-text"
    aSlackMarkdownText = SlackMarkdownText "markdown-text"
    aSlackSection =
      SlackSection
        { slackSectionText = Just "section-text"
        , slackSectionBlockId = Just (NonEmptyText "section-block-id")
        , slackSectionFields = Just ["field-0", "field-1", "field-2"]
        , slackSectionAccessory = Just aSlackAccessory
        }

  describe "SlackAccessory" do
    jsonRoundtrips aSlackAccessory

  describe "SlackAction" do
    jsonRoundtrips aSlackAction

  describe "SlackBlock" do
    describe "SlackBlockSection" do
      jsonRoundtrips aSlackBlockSection

    describe "SlackBlockImage" do
      jsonRoundtrips aSlackBlockImage

    describe "SlackBlockContext" do
      jsonRoundtrips aSlackBlockContext

    describe "SlackBlockDivider" do
      jsonRoundtrips SlackBlockDivider

    -- SlackBlock's ToJSON instance is lossy; SlackBlockRichText values get
    -- encoded as '{}'
    --
    -- describe "SlackBlockRichText" do
    --   jsonRoundtrips aSlackBlockRichText

    describe "SlackBlockActions" do
      jsonRoundtrips aSlackBlockActions

    describe "SlackBlockHeader" do
      jsonRoundtrips aSlackBlockHeader

  describe "SlackConfirmObject" do
    jsonRoundtrips aSlackConfirmObject

  describe "SlackContent" do
    describe "SlackContentText" do
      jsonRoundtrips aSlackContentText

    describe "SlackContentImage" do
      jsonRoundtrips aSlackContentImage

  describe "SlackContext" do
    jsonRoundtrips aSlackContext

  describe "SlackMessage" do
    jsonRoundtrips aSlackMessage

  describe "SlackPlainTextOnly" do
    jsonRoundtrips aSlackPlainTextOnly

  describe "SlackTextObject" do
    describe "SlackPlainText" do
      jsonRoundtrips aSlackPlainText

    describe "SlackMarkdownText" do
      jsonRoundtrips aSlackMarkdownText

-- Untestable for roundtripping:
--
--  FromJSON only
--    - RichItem
--    - RichStyle
--    - RichText
--    - RichTextSectionItem
--    - SlackActionComponent
--    - SlackActionResponse
--    - SlackInteractivePayload
--    - SlackInteractiveResponseResponse
--
--  ToJSON only
--    - SlackInteractiveResponse
--    - SlackText
--
--  Note also that SlackBlock's encoding is lossy, encoding
--  SlackBlockRichText as '{}', so that case is not tested
--  for round-tripping
