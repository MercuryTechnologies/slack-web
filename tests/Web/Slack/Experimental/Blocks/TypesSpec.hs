module Web.Slack.Experimental.Blocks.TypesSpec where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Data.List qualified as List
import Data.StringVariants.NonEmptyText.Internal (pattern NonEmptyText)
import Refined.Unsafe (reallyUnsafeRefine)
import TestImport
import Web.Slack.Common (ConversationId (..))
import Web.Slack.Experimental.Blocks.Types

jsonRoundtrips :: (Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Spec
jsonRoundtrips a = do
  it "can decode its own json encoding" do
    (Aeson.fromJSON . Aeson.toJSON) a `shouldBe` Aeson.Success a

plainJSON :: Text -> Aeson.Value
plainJSON text = object ["type" .= ("plain_text" :: Text), "text" .= text]

optionJSON :: Aeson.Value
optionJSON = object ["text" .= plainJSON "Open", "value" .= ("open" :: Text)]

sectionJSON :: Aeson.Value -> Aeson.Value
sectionJSON accessory = object ["type" .= ("section" :: Text), "text" .= object ["type" .= ("mrkdwn" :: Text), "text" .= ("Choose" :: Text)], "accessory" .= accessory]

-- Decode and re-encode a wire fixture, rather than comparing two codecs which
-- might agree on the same incorrect field names or nested shape.
preservesJSON :: forall a. (FromJSON a, ToJSON a) => Aeson.Value -> Expectation
preservesJSON json = case Aeson.fromJSON @a json of
  Aeson.Error err -> expectationFailure err
  Aeson.Success result -> toJSON result `shouldBe` json

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

  describe "menu elements and placements" do
    let actionId = SlackActionId $ NonEmptyText "choose"
        option = SlackSelectOption "Open" "open" Nothing
        options = SlackSelectOptions $ reallyUnsafeRefine [option]
        overflowOptions = SlackOverflowOptions $ reallyUnsafeRefine [SlackOverflowOption "Open" "open" Nothing Nothing]
    for_ [Nothing, Just actionId] \identifier -> do
      let buttonElement = (button actionId "Open" buttonSettings) {slackButtonActionId = identifier}
          overflowElement = SlackOverflowElement identifier overflowOptions Nothing
          staticElement = SlackStaticSelectElement identifier options Nothing Nothing Nothing Nothing
          externalElement = SlackExternalSelectElement identifier Nothing Nothing Nothing Nothing Nothing
          idFields = maybe [] (\i -> ["action_id" .= i]) identifier
          cases =
            [ ("button", object $ ["type" .= ("button" :: Text), "text" .= plainJSON "Open"] <> idFields, ActionsButton buttonElement, SectionButton buttonElement)
            , ("overflow", object $ ["type" .= ("overflow" :: Text), "options" .= [optionJSON]] <> idFields, ActionsOverflow overflowElement, SectionOverflow overflowElement)
            , ("static select", object $ ["type" .= ("static_select" :: Text), "options" .= [optionJSON]] <> idFields, ActionsStaticSelect staticElement, SectionStaticSelect staticElement)
            , ("external select", object $ ["type" .= ("external_select" :: Text)] <> idFields, ActionsExternalSelect externalElement, SectionExternalSelect externalElement)
            ]
      for_ cases \(name, json, action, accessory) -> describe (name <> " with ID " <> show identifier) do
        it "shares the concrete element's wire format across placements" do
          Aeson.fromJSON @SlackActionsElement json `shouldBe` Aeson.Success action
          Aeson.fromJSON @SlackSectionAccessory json `shouldBe` Aeson.Success accessory
          toJSON action `shouldBe` json
          toJSON accessory `shouldBe` json
        it "decodes the accessory as part of a section" do
          Aeson.fromJSON @SlackBlock (sectionJSON json)
            `shouldBe` Aeson.Success (SlackBlockSection (SlackSection (Just "Choose") Nothing Nothing (Just accessory)))
      describe "concrete codecs" do
        jsonRoundtrips buttonElement
        jsonRoundtrips overflowElement
        jsonRoundtrips staticElement
        jsonRoundtrips externalElement

    it "checks the discriminator even when decoding a concrete record directly" do
      let wrongKind = object ["type" .= ("overflow" :: Text), "text" .= plainJSON "Open"]
      Aeson.fromJSON @SlackButtonElement wrongKind `shouldSatisfy` \case Aeson.Error _ -> True; _ -> False
      Aeson.eitherDecode @SlackExternalSelectElement "{\"type\":\"button\"}" `shouldSatisfy` isLeft
      Aeson.eitherDecode @SlackStaticSelectElement "{\"type\":\"overflow\",\"options\":[]}" `shouldSatisfy` isLeft
      Aeson.eitherDecode @SlackOverflowElement "{\"type\":\"static_select\",\"options\":[]}" `shouldSatisfy` isLeft

    it "retains button settings and overflow option URLs" do
      let confirmation = confirm confirmAreYouSure
          fullButton = button actionId "Open" buttonSettings {buttonUrl = setting (NonEmptyText "https://example.com"), buttonValue = setting (NonEmptyText "open"), buttonStyle = setting SlackStylePrimary, buttonConfirm = setting confirmation}
          fullOption = SlackOverflowOption "Open" "open" (Just "View item") (Just $ NonEmptyText "https://example.com")
          fullOverflow = overflow actionId (SlackOverflowOptions $ reallyUnsafeRefine [fullOption]) overflowSettings {overflowConfirm = setting confirmation}
      Aeson.fromJSON (toJSON fullButton) `shouldBe` Aeson.Success fullButton
      Aeson.fromJSON (toJSON fullOverflow) `shouldBe` Aeson.Success fullOverflow
      toJSON fullOption `shouldBe` object ["text" .= plainJSON "Open", "value" .= ("open" :: Text), "description" .= plainJSON "View item", "url" .= ("https://example.com" :: Text)]

  describe "menu builders" do
    let identifier = SlackActionId (NonEmptyText "choose")
        option = SlackSelectOption "Open" "open" Nothing
        options = SlackSelectOptions (reallyUnsafeRefine [option])
        overflowOptions = SlackOverflowOptions (reallyUnsafeRefine [SlackOverflowOption "Open" "open" Nothing Nothing])
        confirmation = confirm confirmAreYouSure
    it "supplies IDs and omits unset optional fields" do
      toJSON (staticSelect identifier options staticSelectSettings)
        `shouldBe` object ["type" .= ("static_select" :: Text), "action_id" .= identifier, "options" .= [optionJSON]]
      toJSON (externalSelect identifier externalSelectSettings)
        `shouldBe` object ["type" .= ("external_select" :: Text), "action_id" .= identifier]
      toJSON (overflow identifier overflowOptions overflowSettings)
        `shouldBe` object ["type" .= ("overflow" :: Text), "action_id" .= identifier, "options" .= [optionJSON]]
    it "retains every explicit setting including false and zero" do
      staticSelect
        identifier
        options
        staticSelectSettings
          { staticSelectInitialOption = setting option
          , staticSelectConfirm = setting confirmation
          , staticSelectFocusOnLoad = setting False
          , staticSelectPlaceholder = setting "Choose"
          }
        `shouldBe` SlackStaticSelectElement (Just identifier) options (Just option) (Just confirmation) (Just False) (Just "Choose")
      externalSelect
        identifier
        externalSelectSettings
          { externalSelectInitialOption = setting option
          , externalSelectMinQueryLength = setting 0
          , externalSelectConfirm = setting confirmation
          , externalSelectFocusOnLoad = setting False
          , externalSelectPlaceholder = setting "Choose"
          }
        `shouldBe` SlackExternalSelectElement (Just identifier) (Just option) (Just 0) (Just confirmation) (Just False) (Just "Choose")
      overflow identifier overflowOptions overflowSettings {overflowConfirm = setting confirmation}
        `shouldBe` SlackOverflowElement (Just identifier) overflowOptions (Just confirmation)
    for_ ["overflow", "static_select", "external_select"] $ \kind ->
      it ("cannot disguise " <> unpack kind <> " as unknown") do
        parseEither (parseJSON @SlackUnknownElement) (object ["type" .= (kind :: Text)]) `shouldSatisfy` isLeft
    for_
      [ object ["type" .= ("overflow" :: Text)]
      , object ["type" .= ("static_select" :: Text)]
      , object ["type" .= ("external_select" :: Text), "min_query_length" .= (-1 :: Int)]
      ]
      $ \malformed ->
        it "reports malformed menu accessories instead of dropping them" do
          parseEither (parseJSON @SlackBlock) (sectionJSON malformed) `shouldSatisfy` isLeft

  describe "menu definitions" do
    let groupJSON = object ["label" .= plainJSON "Status", "options" .= [optionJSON]]
        confirmation = object ["title" .= plainJSON "Continue?", "text" .= plainJSON "Change item?", "confirm" .= plainJSON "Yes", "deny" .= plainJSON "No"]
        optionalFields = ["initial_option" .= optionJSON, "confirm" .= confirmation, "focus_on_load" .= False, "placeholder" .= plainJSON "Choose"]
    it "preserves grouped options and optional static-menu settings" do
      preservesJSON @SlackStaticSelectElement (object $ ["type" .= ("static_select" :: Text), "option_groups" .= [groupJSON]] <> optionalFields)
    it "preserves external-menu settings, including a zero query length" do
      preservesJSON @SlackExternalSelectElement (object $ ["type" .= ("external_select" :: Text), "min_query_length" .= (0 :: Int)] <> optionalFields)
    for_
      [ ("neither source", object ["type" .= ("static_select" :: Text)])
      , ("both sources", object ["type" .= ("static_select" :: Text), "options" .= [optionJSON], "option_groups" .= [groupJSON]])
      , ("group without label", object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["options" .= [optionJSON]]]])
      , ("group without options", object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["label" .= plainJSON "Status"]]])
      , ("option without value", object ["type" .= ("static_select" :: Text), "options" .= [object ["text" .= plainJSON "Open"]]])
      , ("negative query length", object ["type" .= ("external_select" :: Text), "min_query_length" .= (-1 :: Int)])
      , ("fractional query length", object ["type" .= ("external_select" :: Text), "min_query_length" .= (1.5 :: Double)])
      ]
      \(name, json) -> it ("rejects " <> name) do
        parseEither (parseJSON @SlackActionsElement) json `shouldSatisfy` isLeft
    let numberedOptions count = [object ["text" .= plainJSON (pack $ show n), "value" .= (pack (show n) :: Text)] | n <- [1 .. count :: Int]]
    for_
      [ ("options", \n -> object ["type" .= ("static_select" :: Text), "options" .= numberedOptions n])
      , ("groups", \n -> object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["label" .= plainJSON (pack $ show i), "options" .= [optionJSON]] | i <- [1 .. n :: Int]]])
      , ("options per group", \n -> object ["type" .= ("static_select" :: Text), "option_groups" .= [object ["label" .= plainJSON "Status", "options" .= numberedOptions n]]])
      ]
      \(name, menuJSON) -> describe name do
        it "preserves generated nonempty lists through the wire codec" $ property $ forAll (chooseInt (1, 100)) \n ->
          fmap toJSON (parseEither (parseJSON @SlackActionsElement) (menuJSON n)) === Right (menuJSON n)
        it "accepts 100 entries" $ preservesJSON @SlackActionsElement (menuJSON 100)
        it "rejects 101 entries" $ parseEither (parseJSON @SlackActionsElement) (menuJSON 101) `shouldSatisfy` isLeft

  describe "overflow option bounds" do
    it "accepts five overflow options" do
      preservesJSON @SlackOverflowElement (object ["type" .= ("overflow" :: Text), "options" .= List.replicate 5 optionJSON])
    for_ [0, 6] \count -> it ("rejects " <> show count <> " overflow options") do
      parseEither (parseJSON @SlackOverflowElement) (object ["type" .= ("overflow" :: Text), "options" .= List.replicate count optionJSON]) `shouldSatisfy` isLeft

  describe "menu interaction results" do
    let selected = SlackSelectedOption "Open" "open"
        response kind fields = object $ ["type" .= (kind :: Text), "block_id" .= ("block" :: Text), "action_id" .= ("choose" :: Text), "action_ts" .= ("1720710212.123456" :: Text)] <> fields
        expected result = SlackActionResponse (NonEmptyText "block") (SlackActionId $ NonEmptyText "choose") result
    for_
      [ ("overflow", OverflowSelected selected)
      , ("static_select", StaticSelectChanged (Just selected))
      , ("external_select", ExternalSelectChanged (Just selected))
      ]
      \(kind, result) -> it ("reads " <> unpack kind <> " selections without a menu definition") do
        let json = response kind ["selected_option" .= optionJSON]
        Aeson.fromJSON json `shouldBe` Aeson.Success (expected result)
    for_ [("static_select", StaticSelectChanged Nothing), ("external_select", ExternalSelectChanged Nothing)] \(kind, result) ->
      it ("retains a cleared " <> unpack kind <> " selection") do
        Aeson.fromJSON (response kind ["selected_option" .= Aeson.Null]) `shouldBe` Aeson.Success (expected result)
    it "decodes selections through a complete interactive payload" do
      let payload =
            object
              [ "type" .= ("block_actions" :: Text)
              , "user" .= object ["id" .= ("U123" :: Text), "username" .= ("alex" :: Text), "name" .= ("Alex" :: Text)]
              , "actions"
                  .= [ response "overflow" ["selected_option" .= optionJSON]
                     , response "static_select" ["selected_option" .= Aeson.Null]
                     , response "external_select" ["selected_option" .= optionJSON]
                     ]
              ]
      fmap (map sarActionResult . sipActions) (parseEither (parseJSON @SlackInteractivePayload) payload)
        `shouldBe` Right [OverflowSelected selected, StaticSelectChanged Nothing, ExternalSelectChanged (Just selected)]
    it "allows an omitted external selection" do
      Aeson.fromJSON (response "external_select" []) `shouldBe` Aeson.Success (expected $ ExternalSelectChanged Nothing)
    for_ ["overflow", "static_select"] \kind -> it ("requires the " <> unpack kind <> " selection field") do
      parseEither (parseJSON @SlackActionResponse) (response kind []) `shouldSatisfy` isLeft
    it "requires an actual item for an overflow click" do
      parseEither (parseJSON @SlackActionResponse) (response "overflow" ["selected_option" .= Aeson.Null]) `shouldSatisfy` isLeft
    it "reads button clicks without treating the result as a definition" do
      Aeson.fromJSON (response "button" ["text" .= plainJSON "Open", "value" .= ("open" :: Text)])
        `shouldBe` Aeson.Success (expected $ ButtonClicked $ SlackButtonClick "Open" (Just "open") Nothing)
    it "preserves unknown result kinds" do
      let json = response "future_action" ["answer" .= (42 :: Int)]
      case Aeson.fromJSON @SlackActionResponse json of
        Aeson.Success SlackActionResponse {sarActionResult = ActionResultUnknown raw} -> toJSON raw `shouldBe` json
        result -> expectationFailure $ show result
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
