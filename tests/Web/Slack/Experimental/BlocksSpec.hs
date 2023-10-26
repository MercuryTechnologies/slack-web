module Web.Slack.Experimental.BlocksSpec where

import Control.Monad.Writer.Strict
import Data.StringVariants (mkNonEmptyText)
import JSONGolden (oneGoldenTestEncode)
import TestImport
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Blocks.Types

-- FIXME(jadel, violet): These builder things should probably be put into a
-- module to make them available, but that's later work

headerBlock :: SlackText -> WriterT [SlackBlock] Identity ()
headerBlock = tell . pure . SlackBlockHeader . SlackPlainTextOnly

sectionBlock :: SlackText -> WriterT [SlackBlock] Identity ()
sectionBlock = tell . pure . SlackBlockSection . slackSectionWithText

sectionBlockWithFields :: SlackText -> [SlackText] -> WriterT [SlackBlock] Identity ()
sectionBlockWithFields text fields =
  tell $
    pure $
      SlackBlockSection $
        SlackSection
          { slackSectionText = Just text
          , slackSectionBlockId = Nothing
          , slackSectionFields = Just fields
          , slackSectionAccessory = Nothing
          }

sectionBlockWithAccessory ::
  SlackText ->
  SlackAction ->
  WriterT [SlackBlock] Identity ()
sectionBlockWithAccessory t b =
  tell $
    pure $
      SlackBlockSection
        SlackSection
          { slackSectionText = Just t
          , slackSectionBlockId = Nothing
          , slackSectionFields = Nothing
          , slackSectionAccessory = Just $ SlackButtonAccessory b
          }

dividerBlock :: WriterT [SlackBlock] Identity ()
dividerBlock = tell . pure $ SlackBlockDivider

contextBlock :: [SlackContent] -> WriterT [SlackBlock] Identity ()
contextBlock = tell . pure . SlackBlockContext . SlackContext

actionsBlock ::
  Maybe SlackBlockId ->
  SlackActionList ->
  WriterT [SlackBlock] Identity ()
actionsBlock a b = tell . pure $ SlackBlockActions a b

slackActionDoNothing :: SlackActionId
slackActionDoNothing = SlackActionId . fromJust . mkNonEmptyText $ "doNothing"

slackActionDoNothing2 :: SlackActionId
slackActionDoNothing2 = SlackActionId . fromJust . mkNonEmptyText $ "doNothing2"

slackActionDoNothing3 :: SlackActionId
slackActionDoNothing3 = SlackActionId . fromJust . mkNonEmptyText $ "doNothing3"

slackShowBlockFormat :: BlockKitBuilderMessage
slackShowBlockFormat =
  BlockKitBuilderMessage $
    execWriter $ do
      headerBlock ("Blah: " <> list ["a", "b", "c"])
      dividerBlock
      sectionBlock (bold $ list ["blah", "blah2", "blah3"])
      sectionBlockWithAccessory
        (monospaced @Text "blah")
        (button slackActionDoNothing ":mag: Look at it" buttonSettings {buttonUrl = google})
      sectionBlockWithAccessory
        (bold $ list ["blah", "blah2", "blah3"])
        (button slackActionDoNothing ":office: Look at it but different" buttonSettings {buttonUrl = google})
      sectionBlock (bold (textMessage "Letters:") <> newline (list ["a", "b", "c"]))
      sectionBlock (list ["blah1", "blah2", "blah3"])
      dividerBlock
      sectionBlock (bold . textMessage $ "blah")
      sectionBlockWithAccessory
        (bold . textMessage $ "blah")
        (button slackActionDoNothing ":bank: Look at it!" buttonSettings {buttonUrl = google})
      sectionBlockWithFields
        (message @Text "Section with text and fields")
        [ (bold $ message @Text "Field 1") <> (newline $ message @Text "Foo")
        , (bold $ message @Text "Field 2") <> (newline $ message @Text "Bar")
        , (bold $ message @Text "Field 3") <> (newline $ message @Text "Baz")
        , (bold $ message @Text "Field 4") <> (newline $ message @Text "Qux")
        ]
      contextBlock [SlackContentText ":key: Context!"]
      dividerBlock
      actionsBlock Nothing $
        toSlackActionList
          ( button slackActionDoNothing ":mag: View" buttonSettings {buttonUrl = google}
          , button slackActionDoNothing2 ":office: View" buttonSettings {buttonUrl = google}
          , button slackActionDoNothing3 ":bank: View" buttonSettings {buttonUrl = google}
          )
  where
    textMessage = message @Text
    list x = textMessage (intercalate ", " x)
    google = OptionalSetting . mkNonEmptyText @3000 $ "https://google.com"

-- | This is used so that we can paste the output of these tests directly into
-- the block kit builder:
-- <https://app.slack.com/block-kit-builder>
newtype BlockKitBuilderMessage = BlockKitBuilderMessage {blocks :: [SlackBlock]}

instance ToJSON BlockKitBuilderMessage where
  toJSON BlockKitBuilderMessage {blocks} = object [("blocks", toJSON blocks)]

spec :: Spec
spec = describe "Slack block builder" do
  oneGoldenTestEncode "simple_blocks" slackShowBlockFormat
