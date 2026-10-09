module Web.Slack.Experimental.Blocks.TypesSpec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Data.StringVariants.NonEmptyText.Internal (pattern NonEmptyText)
import Refined.Unsafe (reallyUnsafeRefine)
import TestImport
import Web.Slack.Common (ConversationId (..))
import Web.Slack.Experimental.Blocks qualified as Blocks
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
      Aeson.eitherDecode @SlackBlock "{\"type\":\"actions\",\"elements\":[{\"type\":\"future_action\",\"action_id\":\"select\"}]}"
        `shouldBe` Left "Error in $.elements[0]: Unknown SlackActionComponent type \"future_action\", must be one of ['button', 'overflow', 'static_select', 'external_select']"

    it "reports the rejected accessory type before requiring action fields" do
      Aeson.eitherDecode @SlackAccessory "{\"type\":\"image\",\"image_url\":\"https://example.com/image.png\",\"alt_text\":\"example\"}"
        `shouldBe` Left "Error in $: Unknown SlackAccessory type \"image\", must be one of ['button', 'overflow', 'static_select', 'external_select']"

    it "reports the rejected accessory type even when action fields are present" do
      Aeson.eitherDecode @SlackAccessory "{\"type\":\"future_action\",\"action_id\":\"select\"}"
        `shouldBe` Left "Error in $: Unknown SlackAccessory type \"future_action\", must be one of ['button', 'overflow', 'static_select', 'external_select']"

  describe "overflow menus" do
    let optionJSON = object ["text" .= object ["type" .= ("plain_text" :: Text), "text" .= ("Open" :: Text)], "value" .= ("open" :: Text)]
        option = SlackOverflowOption "Open" "open" Nothing Nothing
        options = SlackOverflowOptions $ reallyUnsafeRefine [option]
        menu = SlackOverflow $ SlackOverflowMenu options Nothing
        menuJSON = object ["type" .= ("overflow" :: Text), "options" .= [optionJSON]]

    it "decodes a menu without an action ID or confirmation" do
      Aeson.fromJSON @SlackAction menuJSON `shouldBe` Aeson.Success (SlackAction Nothing menu)

    it "omits absent optional fields when encoding" do
      toJSON (SlackAction Nothing menu) `shouldBe` menuJSON

    it "builds a menu with an action ID and no confirmation by default" do
      toJSON (Blocks.overflow (SlackActionId $ NonEmptyText "more") options Blocks.overflowSettings)
        `shouldBe` object ["type" .= ("overflow" :: Text), "action_id" .= ("more" :: Text), "options" .= [optionJSON]]

    it "decodes a section accessory without an action ID" do
      Aeson.fromJSON @SlackAccessory menuJSON
        `shouldBe` Aeson.Success (SlackOverflowAccessory $ SlackAction Nothing menu)

    it "requires options" do
      Aeson.eitherDecode @SlackAction "{\"type\":\"overflow\"}" `shouldSatisfy` isLeft

    it "requires each option's value" do
      Aeson.eitherDecode @SlackAction "{\"type\":\"overflow\",\"options\":[{\"text\":{\"type\":\"plain_text\",\"text\":\"Open\"}}]}" `shouldSatisfy` isLeft

    it "accepts five options" do
      let fiveOptions = [option {slackOverflowOptionValue = pack (show n)} | n <- [1 .. 5 :: Int]]
      Aeson.fromJSON @SlackOverflowOptions (toJSON fiveOptions)
        `shouldBe` Aeson.Success (SlackOverflowOptions $ reallyUnsafeRefine fiveOptions)

    for_ [0, 6] \count ->
      it ("rejects " <> show count <> " options") do
        parseEither (parseJSON @SlackAction) (object ["type" .= ("overflow" :: Text), "options" .= (replicate count optionJSON :: [Aeson.Value])])
          `shouldSatisfy` isLeft

    let fullOption = option {slackOverflowOptionDescription = Just "View the item", slackOverflowOptionUrl = Just (NonEmptyText "https://example.com/item")}
        fullMenu = Blocks.overflow (SlackActionId $ NonEmptyText "more") (SlackOverflowOptions $ reallyUnsafeRefine [fullOption]) Blocks.overflowSettings {Blocks.overflowConfirm = setting $ confirm confirmAreYouSure}
    it "applies the confirmation setting" do
      slackActionComponent fullMenu `shouldBe` SlackOverflow (SlackOverflowMenu (SlackOverflowOptions $ reallyUnsafeRefine [fullOption]) (Just $ confirm confirmAreYouSure))
    describe "option with a description and URL" do
      jsonRoundtrips fullOption
    describe "action with an ID and confirmation" do
      jsonRoundtrips fullMenu
    describe "section accessory" do
      jsonRoundtrips $ SlackOverflowAccessory fullMenu

  describe "select menus" do
    let textJSON text = object ["type" .= ("plain_text" :: Text), "text" .= (text :: Text)]
        optionJSON = object ["text" .= textJSON "Open", "value" .= ("open" :: Text), "description" .= textJSON "Active item"]
        option = SlackSelectOption "Open" "open" (Just "Active item")
        groupJSON = object ["label" .= textJSON "Status", "options" .= [optionJSON]]
        optionGroup = SlackSelectOptionGroup "Status" $ reallyUnsafeRefine [option]
        staticOptions = SlackSelectOptions $ reallyUnsafeRefine [option]
        staticGroups = SlackSelectOptionGroups $ reallyUnsafeRefine [optionGroup]
        staticMenu source = SlackStaticSelectMenu source Nothing Nothing Nothing Nothing
        externalMenu = SlackExternalSelectMenu Nothing Nothing Nothing Nothing Nothing
        checkMenu caseName json action accessory = describe caseName do
          it "decodes the documented fields" do
            Aeson.fromJSON @SlackAction json `shouldBe` Aeson.Success action
          it "encodes the documented fields, omitting absent ones" do
            toJSON action `shouldBe` json
          it "also decodes as a section accessory" do
            Aeson.fromJSON @SlackAccessory json `shouldBe` Aeson.Success (accessory action)

    checkMenu
      "static options without optional fields"
      (object ["type" .= ("static_select" :: Text), "options" .= [optionJSON]])
      (SlackAction Nothing $ SlackStaticSelect $ staticMenu staticOptions)
      SlackStaticSelectAccessory

    checkMenu
      "static option groups without optional fields"
      (object ["type" .= ("static_select" :: Text), "option_groups" .= [groupJSON]])
      (SlackAction Nothing $ SlackStaticSelect $ staticMenu staticGroups)
      SlackStaticSelectAccessory

    checkMenu
      "external source without an options list or optional fields"
      (object ["type" .= ("external_select" :: Text)])
      (SlackAction Nothing $ SlackExternalSelect externalMenu)
      SlackExternalSelectAccessory

    let confirmationJSON = object ["title" .= textJSON "Continue?", "text" .= textJSON "Change this item?", "confirm" .= textJSON "Yes", "deny" .= textJSON "No"]
        confirmation = SlackConfirmObject "Continue?" (SlackPlainText "Change this item?") "Yes" "No" Nothing
        commonFields =
          [ "action_id" .= ("choose" :: Text)
          , "initial_option" .= optionJSON
          , "confirm" .= confirmationJSON
          , "focus_on_load" .= False
          , "placeholder" .= textJSON "Choose an item"
          ]
        actionId = SlackActionId $ NonEmptyText "choose"

    for_ [("flat options", staticOptions, "options" .= [optionJSON]), ("option groups", staticGroups, "option_groups" .= [groupJSON])] \(caseName, source, sourceJSON) ->
      checkMenu
        ("static builder defaults with " <> caseName)
        (object ["type" .= ("static_select" :: Text), "action_id" .= ("choose" :: Text), sourceJSON])
        (Blocks.staticSelect actionId source Blocks.staticSelectSettings)
        SlackStaticSelectAccessory

    checkMenu
      "external builder defaults"
      (object ["type" .= ("external_select" :: Text), "action_id" .= ("choose" :: Text)])
      (Blocks.externalSelect actionId Blocks.externalSelectSettings)
      SlackExternalSelectAccessory

    checkMenu
      "static menu with optional fields"
      (object $ ["type" .= ("static_select" :: Text), "option_groups" .= [groupJSON]] <> commonFields)
      (Blocks.staticSelect actionId staticGroups Blocks.staticSelectSettings {Blocks.staticSelectInitialOption = setting option, Blocks.staticSelectConfirm = setting confirmation, Blocks.staticSelectFocusOnLoad = setting False, Blocks.staticSelectPlaceholder = setting "Choose an item"})
      SlackStaticSelectAccessory

    checkMenu
      "external menu with optional fields and zero minimum query length"
      (object $ ["type" .= ("external_select" :: Text), "min_query_length" .= (0 :: Int)] <> commonFields)
      (Blocks.externalSelect actionId Blocks.externalSelectSettings {Blocks.externalSelectInitialOption = setting option, Blocks.externalSelectMinQueryLength = setting 0, Blocks.externalSelectConfirm = setting confirmation, Blocks.externalSelectFocusOnLoad = setting False, Blocks.externalSelectPlaceholder = setting "Choose an item"})
      SlackExternalSelectAccessory

    for_
      [ ("missing options", object ["type" .= ("static_select" :: Text)])
      , ("both options and groups", object ["type" .= ("static_select" :: Text), "options" .= [optionJSON], "option_groups" .= [groupJSON]])
      , ("group without a label", object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["options" .= [optionJSON]]]])
      , ("group without options", object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["label" .= textJSON "Status"]]])
      , ("option without a value", object ["type" .= ("static_select" :: Text), "options" .= [object ["text" .= textJSON "Open"]]])
      , ("negative query length", object ["type" .= ("external_select" :: Text), "min_query_length" .= (-1 :: Int)])
      , ("fractional query length", object ["type" .= ("external_select" :: Text), "min_query_length" .= (1.5 :: Double)])
      ]
      \(caseName, json) -> it ("rejects " <> caseName) do
        parseEither (parseJSON @SlackAction) json `shouldSatisfy` isLeft

    let numberedOption n = object ["text" .= textJSON (pack $ show n), "value" .= (pack (show n) :: Text)]
        numberedOptions count = [numberedOption n | n <- [1 .. count :: Int]]
        numberedGroups count = [object ["label" .= textJSON (pack $ show n), "options" .= [numberedOption n]] | n <- [1 .. count :: Int]]
    for_
      [ ("options", \count -> object ["type" .= ("static_select" :: Text), "options" .= numberedOptions count])
      , ("groups", \count -> object ["type" .= ("static_select" :: Text), "option_groups" .= numberedGroups count])
      , ("options per group", \count -> object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["label" .= textJSON "Status", "options" .= numberedOptions count]]])
      ]
      \(caseName, menuJSON) -> describe caseName do
        it "accepts the documented maximum of 100" do
          case Aeson.fromJSON @SlackAction (menuJSON 100) of
            Aeson.Error err -> expectationFailure err
            Aeson.Success action -> toJSON action `shouldBe` menuJSON 100
        it "rejects 101" do
          parseEither (parseJSON @SlackAction) (menuJSON 101) `shouldSatisfy` isLeft

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
      do Just $ SlackActionId $ NonEmptyText "action-id"
      do aSlackButton
    aSlackActionList =
      SlackActionList
        . reallyUnsafeRefine
        $ [ aSlackAction
          , SlackAction
              do Just $ SlackActionId $ NonEmptyText "another-action-id"
              do
                SlackButton
                  $ SlackButtonElement
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
        $ SlackButtonElement
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
