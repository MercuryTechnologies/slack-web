module Web.Slack.Experimental.BlocksSpec where

import Data.StringVariants (mkNonEmptyText)
import JSONGolden (oneGoldenTestEncode)
import TestImport
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Blocks.Builder (BlockKitBuilderMessage (..), actionsBlock, contextBlock, dividerBlock, headerBlock, runBlockBuilder, sectionBlock, sectionBlockWithAccessory, sectionBlockWithFields)
import Web.Slack.Experimental.Blocks.Types

slackActionDoNothing :: SlackActionId
slackActionDoNothing = SlackActionId . fromJust . mkNonEmptyText $ "doNothing"

slackActionDoNothing2 :: SlackActionId
slackActionDoNothing2 = SlackActionId . fromJust . mkNonEmptyText $ "doNothing2"

slackActionDoNothing3 :: SlackActionId
slackActionDoNothing3 = SlackActionId . fromJust . mkNonEmptyText $ "doNothing3"

slackShowBlockFormat :: BlockKitBuilderMessage
slackShowBlockFormat =
  BlockKitBuilderMessage
    $ runIdentity
    . runBlockBuilder
    $ do
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
      actionsBlock Nothing
        $ toSlackActionList
          ( button slackActionDoNothing ":mag: View" buttonSettings {buttonUrl = google}
          , button slackActionDoNothing2 ":office: View" buttonSettings {buttonUrl = google}
          , button slackActionDoNothing3 ":bank: View" buttonSettings {buttonUrl = google}
          )
  where
    textMessage = message @Text
    list x = textMessage (intercalate ", " x)
    google = OptionalSetting . mkNonEmptyText @3000 $ "https://google.com"

spec :: Spec
spec = describe "Slack block builder" do
  oneGoldenTestEncode "simple_blocks" slackShowBlockFormat
