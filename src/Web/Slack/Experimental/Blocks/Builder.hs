-- | A builder for Slack blocks
--
-- @since 2.1.0.0
module Web.Slack.Experimental.Blocks.Builder where

import Control.Monad.Writer.Strict
import Web.Slack.Experimental.Blocks.Types (SlackAccessory (..), SlackAction, SlackActionList, SlackBlock (..), SlackBlockId, SlackContent, SlackContext (..), SlackPlainTextOnly (..), SlackSection (..), SlackText, slackSectionWithText)
import Web.Slack.Prelude

type BlockBuilder = WriterT [SlackBlock]

-- | Runs a block builder, yielding a result
runBlockBuilder :: (Monad m) => BlockBuilder m () -> m [SlackBlock]
runBlockBuilder = execWriterT

-- | Header block.
--
-- The text is max 150 characters long.
--
-- <https://api.slack.com/reference/block-kit/blocks#header>
--
-- @since 2.1.0.0
headerBlock :: (Monad m) => SlackText -> BlockBuilder m ()
headerBlock = tell . pure . SlackBlockHeader . SlackPlainTextOnly

-- | Section block. Similar in concept to a p or div tag in HTML.
--
-- <https://api.slack.com/reference/block-kit/blocks#section>
--
-- @since 2.1.0.0
sectionBlock :: (Monad m) => SlackText -> BlockBuilder m ()
sectionBlock = tell . pure . SlackBlockSection . slackSectionWithText

-- | Section block. Similar in concept to a p or div tag in HTML.
--
-- <https://api.slack.com/reference/block-kit/blocks#section>
--
-- @since 2.1.0.0
sectionBlockWithFields :: (Monad m) => SlackText -> [SlackText] -> BlockBuilder m ()
sectionBlockWithFields text fields =
  tell
    $ pure
    $ SlackBlockSection
    $ SlackSection
      { slackSectionText = Just text
      , slackSectionBlockId = Nothing
      , slackSectionFields = Just fields
      , slackSectionAccessory = Nothing
      }

-- | Section block. Similar in concept to a p or div tag in HTML.
--
-- <https://api.slack.com/reference/block-kit/blocks#section>
--
-- @since 2.1.0.0
sectionBlockWithAccessory ::
  (Monad m) =>
  SlackText ->
  SlackAction ->
  BlockBuilder m ()
sectionBlockWithAccessory t b =
  tell
    $ pure
    $ SlackBlockSection
      SlackSection
        { slackSectionText = Just t
        , slackSectionBlockId = Nothing
        , slackSectionFields = Nothing
        , slackSectionAccessory = Just $ SlackButtonAccessory b
        }

-- | Horizontal line. Similar to an html @hr@ tag.
--
-- <https://api.slack.com/reference/block-kit/blocks#divider>
--
-- @since 2.1.0.0
dividerBlock :: (Monad m) => BlockBuilder m ()
dividerBlock = tell . pure $ SlackBlockDivider

-- | Context block: smaller, grey, text, and rendered inline. Like a @span@ in HTML.
--
-- <https://api.slack.com/reference/block-kit/blocks#context>
contextBlock :: (Monad m) => [SlackContent] -> BlockBuilder m ()
contextBlock = tell . pure . SlackBlockContext . SlackContext

-- | Inline container for interactive actions.
--
-- <https://api.slack.com/reference/block-kit/blocks#actions>
actionsBlock ::
  (Monad m) =>
  Maybe SlackBlockId ->
  SlackActionList ->
  BlockBuilder m ()
actionsBlock a b = tell . pure $ SlackBlockActions a b

-- | This is used for testing purposes so that you can paste these into the
-- Block Kit builder and have the expected format:
--
-- <https://app.slack.com/block-kit-builder>
--
-- @since 2.1.0.0
newtype BlockKitBuilderMessage = BlockKitBuilderMessage {blocks :: [SlackBlock]}

instance ToJSON BlockKitBuilderMessage where
  toJSON BlockKitBuilderMessage {blocks} = object [("blocks", toJSON blocks)]
