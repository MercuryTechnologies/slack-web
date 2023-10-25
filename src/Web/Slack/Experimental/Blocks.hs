module Web.Slack.Experimental.Blocks
  ( -- * General Slack Messages
    SlackText,
    (<+>),
    parens,
    brackets,
    angleBrackets,
    ticks,
    codeBlock,
    bold,
    italic,
    newline,
    unorderedList,
    link,
    monospaced,
    mentionUser,
    isSubStringOf,
    SlackImage (..),
    SlackMessage,
    Markdown (..),
    Image (..),
    context,
    textToMessage,
    prefixFirstSlackMessage,
    mentionUserGroupById,
    textToContext,
    slackMessage,
    SlackBlock (..),

    -- ** Blocks' rich text formatting (receive only!)
    RichItem (..),
    RichStyle (..),
    RichLinkAttrs (..),
    RichTextSectionItem (..),
    RichText (..),

    -- * Rendered messages
    RenderedSlackMessage (..),
    render,

    -- * Introduction to Slack Interactive Messages
    -- $interactive

    -- * Creating Slack Interactive Messages
    actions,
    actionsWithBlockId,
    SlackActionId (..),
    SlackBlockId,
    setting,
    emptySetting,
    SlackStyle (..),
    plaintext,
    plaintextonly,
    mrkdwn,
    button,
    buttonSettings,
    ButtonSettings
      ( buttonUrl,
        buttonValue,
        buttonStyle,
        buttonConfirm
      ),
    confirm,
    confirmAreYouSure,
    ConfirmSettings
      ( confirmTitle,
        confirmText,
        confirmConfirm,
        confirmDeny,
        confirmStyle
      ),

    -- * Responding to Slack Interactive Messages
    SlackInteractiveResponse (..),
  )
where

import Data.Aeson.Text (encodeToLazyText)
import Data.Text qualified as T
import Web.Slack.Experimental.Blocks.Types
import Web.Slack.Prelude
import Web.Slack.Types

-- | Concatenate two 'SlackText's with a space separator.
(<+>) :: SlackText -> SlackText -> SlackText
x <+> y = x <> " " <> y

parens :: SlackText -> SlackText
parens x = "(" <> x <> ")"

brackets :: SlackText -> SlackText
brackets x = "[" <> x <> "]"

angleBrackets :: SlackText -> SlackText
angleBrackets x = "<" <> x <> ">"

ticks :: SlackText -> SlackText
ticks x = "`" <> x <> "`"

-- | Render a 'Slack' renderable value with ticks around it. Alias for @ticks . message@
monospaced :: Slack a => a -> SlackText
monospaced = ticks . message

codeBlock :: SlackText -> SlackText
codeBlock x = "```\n" <> x <> "\n```"

bold :: SlackText -> SlackText
bold x = "*" <> x <> "*"

italic :: SlackText -> SlackText
italic x = "_" <> x <> "_"

-- | Add a newline before a 'SlackText'.
newline :: SlackText -> SlackText
newline x = "\n" <> x

-- | Render an unordered (bulleted) list
unorderedList :: [SlackText] -> SlackText
unorderedList = mconcat . fmap (\t -> newline (message @Text "- " <> t))

-- | https://api.slack.com/reference/surfaces/formatting#mentioning-users
mentionUser :: UserId -> SlackText
mentionUser slackUserId = message $ "<@" <> unUserId slackUserId <> ">"

-- | https://api.slack.com/reference/surfaces/formatting#mentioning-groups
mentionUserGroupById :: SlackText -> SlackText
mentionUserGroupById userGroupId = angleBrackets $ "!subteam^" <> userGroupId

isSubStringOf :: Text -> SlackText -> Bool
needle `isSubStringOf` (SlackText haystack) = needle `isInfixOf` concat haystack

-- | RenderedSlackMessage contains the original SlackMessage, the rendered version, and a
--   boolean indicating whether truncation was done.
--
--   Usage:
--
--   @
--      let
--        msg =
--        (mkPostMsgReq channel "")
--          { postMsgReqBlocks = Just blocks
--          , postMsgReqThreadTs = mThreadTs
--          }
--    chatPostMessage msg
--   @
data RenderedSlackMessage = RenderedSlackMessage
  { _originalMessage :: SlackMessage
  , _renderedMessage :: Text
  , _truncated :: Bool
  }

render :: SlackMessage -> RenderedSlackMessage
render sm =
  let (truncatedSm, isTruncated) = truncateSlackMessage sm
   in RenderedSlackMessage sm (cs . encodeToLazyText . toJSON $ truncatedSm) isTruncated

-- | Slack will fail if any text block is over 3000 chars.
--   truncateSlackMessage will truncate all text blocks and also return an bool
--   indicating whether any truncation was done.
truncateSlackMessage :: SlackMessage -> (SlackMessage, Bool)
truncateSlackMessage (SlackMessage blocks) =
  let (truncatedBlocks, isTruncateds) = unzip $ map truncateSlackBlock blocks
   in (SlackMessage truncatedBlocks, or isTruncateds)

-- | TODO(maxh) Update this for the SlackSection
truncateSlackBlock :: SlackBlock -> (SlackBlock, Bool)
truncateSlackBlock sb@(SlackBlockSection SlackSection {..}) =
  let texts = maybe mempty unSlackTexts slackSectionText
      messageLength = sum $ map T.length texts
      lengthLimit = 3000
      truncationMessage = "\n...Rest of message truncated for slack\n"
      truncationMessageLength = T.length truncationMessage
      truncateTexts ts = take (lengthLimit - truncationMessageLength) (concat ts)
      truncatedSection =
        SlackBlockSection
          SlackSection
            { slackSectionText = Just $ SlackText [truncateTexts texts <> "\n...Rest of message truncated for slack\n"]
            , slackSectionAccessory
            , slackSectionBlockId
            , slackSectionFields
            }
   in if messageLength > lengthLimit
        then (truncatedSection, True)
        else (sb, False)
-- possible to also truncate SlackContexts, but we never put long strings in there.
truncateSlackBlock x = (x, False)

prefixFirstSlackBlockSection :: Text -> [SlackBlock] -> ([SlackBlock], Bool)
prefixFirstSlackBlockSection prefix (SlackBlockSection SlackSection {..} : sbs) =
  let prefixedSection =
        SlackBlockSection
          SlackSection
            { slackSectionText = mappend (message prefix) <$> slackSectionText
            , slackSectionBlockId
            , slackSectionFields
            , slackSectionAccessory
            }
   in (prefixedSection : sbs, True)
prefixFirstSlackBlockSection prefix (sb : sbs) =
  let (prefixedSbs, match) = prefixFirstSlackBlockSection prefix sbs
   in (sb : prefixedSbs, match)
prefixFirstSlackBlockSection _ [] =
  ([], False)

prefixFirstSlackMessage :: Text -> [SlackMessage] -> [SlackMessage]
prefixFirstSlackMessage prefix (sm : sms) =
  let SlackMessage slackBlocks = sm
      (prefixedSlackBlocks, match) = prefixFirstSlackBlockSection prefix slackBlocks
   in if match
        then SlackMessage prefixedSlackBlocks : sms
        else sm : prefixFirstSlackMessage prefix sms
prefixFirstSlackMessage _ [] = []

-- | Concatenate a list of 'SlackText' into a single block, and wrap it up as a full message
slackMessage :: [SlackText] -> SlackMessage
slackMessage = SlackMessage . pure . SlackBlockSection . slackSectionWithText . mconcat

-- $interactive
--
-- = Slack Interactive Messages
--
-- First, familiarize yourself with [Slack's Interactivity documentation](https://api.slack.com/interactivity).
-- Currently we only support [Block Kit interactive components](https://api.slack.com/interactivity/components),
-- i.e. components like buttons attached to messages.
--
-- To make an Slack message interactive, you need to include an \"Actions\" block that has one or more interactive components.
-- These should generally only be created using the builder functions such as 'actions' and 'button'. Consumers of this module
-- should avoid directly importing "Web.Slack.Experimental.Blocks.Types".
