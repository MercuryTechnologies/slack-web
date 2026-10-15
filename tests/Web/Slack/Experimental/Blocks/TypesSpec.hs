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

  describe "element definitions and placements" do
    for_ [Nothing, Just (SlackActionId (NonEmptyText "open"))] $ \identifier -> do
      let element = (button (SlackActionId (NonEmptyText "open")) "Open" buttonSettings) {slackButtonActionId = identifier}
      it ("shares the button wire format across placements, with ID " <> show identifier) do
        toJSON (ActionsButton element) `shouldBe` toJSON element
        toJSON (SectionButton element) `shouldBe` toJSON element
        Aeson.fromJSON (toJSON element) `shouldBe` Aeson.Success (ActionsButton element)
        Aeson.fromJSON (toJSON element) `shouldBe` Aeson.Success (SectionButton element)
      jsonRoundtrips element
    it "rejects a wrong discriminator when parsing a concrete button" do
      Aeson.eitherDecode @SlackButtonElement "{\"type\":\"future_element\",\"text\":{\"type\":\"plain_text\",\"text\":\"Open\"}}" `shouldSatisfy` isLeft

  describe "unknown and malformed elements" do
    let unknown = object ["type" .= ("future_element" :: Text), "extra" .= object ["value" .= (42 :: Int)]]
    it "preserves unsupported actions and accessories exactly" do
      fmap toJSON (parseEither (parseJSON @SlackActionsElement) unknown) `shouldBe` Right unknown
      fmap toJSON (parseEither (parseJSON @SlackSectionAccessory) unknown) `shouldBe` Right unknown
      let section = object ["type" .= ("section" :: Text), "accessory" .= unknown]
      case parseEither (parseJSON @SlackBlock) section of
        Right (SlackBlockSection value) -> fmap toJSON (slackSectionAccessory value) `shouldBe` Just unknown
        result -> expectationFailure (show result)
    it "does not drop malformed known accessories and retains their error path" do
      Aeson.eitherDecode @SlackBlock "{\"type\":\"section\",\"accessory\":{\"type\":\"button\"}}"
        `shouldBe` Left "Error in $.accessory: key \"text\" not found"
    for_ ["{}", "[]", "{\"type\":42}", "{\"type\":\"button\"}"] $ \json ->
      it "cannot disguise malformed or supported elements as unknown" do
        Aeson.eitherDecode @SlackUnknownElement json `shouldSatisfy` isLeft

  describe "interaction results" do
    it "parses a button click independently of definition-only constraints" do
      let click = object ["type" .= ("button" :: Text), "block_id" .= ("block" :: Text), "action_id" .= ("open" :: Text), "text" .= plaintextonly ("Open" :: Text), "style" .= ("not-a-definition-style" :: Text), "value" .= ("item" :: Text)]
      parseEither (parseJSON @SlackActionResponse) click
        `shouldBe` Right (SlackActionResponse (NonEmptyText "block") (SlackActionId (NonEmptyText "open")) (ButtonClicked (SlackButtonClick (plaintextonly ("Open" :: Text)) (Just "item") Nothing)))
    it "retains unsupported interaction results" do
      let unknown = object ["type" .= ("future_interaction" :: Text), "value" .= ("item" :: Text)]
      case parseEither (parseJSON @SlackActionResult) unknown of
        Right (ActionResultUnknown value) -> toJSON value `shouldBe` unknown
        result -> expectationFailure (show result)

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
    aSlackAccessory = SectionButton aSlackButton
    aSlackAction = ActionsButton aSlackButton
    aSlackActionList =
      SlackActionList
        . reallyUnsafeRefine
        $ [aSlackAction, ActionsButton (button (SlackActionId (NonEmptyText "another-action-id")) "another-button-text" buttonSettings)]
    aSlackBlockSection = SlackBlockSection aSlackSection
    aSlackBlockImage = SlackBlockImage aSlackImage
    aSlackBlockContext = SlackBlockContext aSlackContext
    aSlackBlockActions = SlackBlockActions
      do Just $ NonEmptyText "block-actions"
      do aSlackActionList
    aSlackBlockHeader = SlackBlockHeader $ SlackPlainTextOnly "block-header"
    aSlackButton =
      SlackButtonElement
        { slackButtonActionId = Just (SlackActionId (NonEmptyText "action-id"))
        , slackButtonText = SlackButtonText (NonEmptyText "button-text")
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

  describe "SlackSectionAccessory" do
    jsonRoundtrips aSlackAccessory

  describe "SlackActionsElement" do
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
