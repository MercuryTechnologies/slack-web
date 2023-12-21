module Web.Slack.Experimental.Blocks.TypesSpec where

import Data.Aeson qualified as Aeson
import Data.StringVariants.NonEmptyText.Internal (pattern NonEmptyText)
import Refined.Unsafe (reallyUnsafeRefine)
import TestImport
import Web.Slack.Experimental.Blocks.Types

jsonRoundtrips :: (Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Spec
jsonRoundtrips a = do
  it "can decode its own json encoding" do
    (Aeson.fromJSON . Aeson.toJSON) a `shouldBe` Aeson.Success a

spec :: Spec
spec = do
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
