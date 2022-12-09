module Web.Slack.Experimental.Blocks.Builder where

import Data.Maybe (fromJust)
import Control.Applicative
import Web.Slack.Prelude
import Control.Monad.Writer.Strict
import Data.StringVariants (mkNonEmptyText)
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Blocks.Types

-- textMessage = message @Text
-- list x = textMessage (intercalate ", " x)

newtype SlackBlockM a = SlackBlockM {unSlackBlockM :: Writer [SlackBlock] a}
  deriving newtype (Functor, Applicative, Monad, MonadWriter [SlackBlock])

composeSlackBlocks :: SlackBlockM () -> SlackMessage --[SlackBlock]
composeSlackBlocks slackBlockWriter = 
  SlackMessage . execWriter $ unSlackBlockM slackBlockWriter

headerBlock :: SlackPlainTextOnly -> SlackBlockM ()
headerBlock slackPlainText = tell [SlackBlockHeader slackPlainText]

sectionBlock :: SlackText -> SlackBlockM ()
sectionBlock slackText = tell [SlackBlockSection slackText Nothing]

sectionBlockWithAccessoryButton ::
  SlackText ->
  SlackAction ->
  SlackBlockM ()
sectionBlockWithAccessoryButton slackText slackAction = tell [SlackBlockSection slackText (Just $ SlackButtonAccessory slackAction)]

dividerBlock :: SlackBlockM ()
dividerBlock = tell [SlackBlockDivider]

contextBlock :: [SlackContent] -> SlackBlockM ()
contextBlock slackContents = tell [SlackBlockContext $ SlackContext slackContents]

actionsBlock ::
  Maybe SlackBlockId ->
  SlackActionList ->
  SlackBlockM ()
actionsBlock mSlackBlockId slackActions = tell [SlackBlockActions mSlackBlockId slackActions]

slackActionDoNothing :: SlackActionId
slackActionDoNothing = SlackActionId . fromJust . mkNonEmptyText $ "doNothing"

-- | This is used so that we can paste the output of these tests directly into
-- the block kit builder:
-- <https://app.slack.com/block-kit-builder>
newtype BlockKitBuilderMessage = BlockKitBuilderMessage {blocks :: [SlackBlock]}

instance ToJSON BlockKitBuilderMessage where
  toJSON BlockKitBuilderMessage {blocks} = object [("blocks", toJSON blocks)]

