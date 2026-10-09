{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Slack.Experimental.Blocks.Types where

import Control.Monad (MonadFail (..))
import Data.Aeson (Object, Result (..), Value (..), fromJSON, withArray)
import Data.Aeson.Types (Pair, (.!=))
import Data.StringVariants
import Data.Vector qualified as V
import Numeric.Natural (Natural)
import Refined
import Refined.Unsafe (reallyUnsafeRefine)
import Web.Slack.AesonUtils
import Web.Slack.Common (ConversationId, UserId)
import Web.Slack.Prelude
import Web.Slack.Types (Emoji)

-- | Class of types that can be turned into part of a Slack Message. 'message'
-- is the primary way of converting primitive and domain-level types into things
-- that can be shown in a slack message.
class Slack a where
  message :: a -> SlackText

newtype SlackText = SlackText {unSlackTexts :: [Text]}
  deriving newtype (Semigroup, Monoid, Eq)

-- | Render a link with optional link text
link :: Text -> Maybe Text -> SlackText
link uri = \case
  Nothing -> message $ "<" <> uri <> ">"
  Just linkText -> message $ "<" <> uri <> "|" <> linkText <> ">"

data SlackPlainTextOnly = SlackPlainTextOnly SlackText
  deriving stock (Eq, Show)

instance IsString SlackPlainTextOnly where
  fromString = SlackPlainTextOnly . message

instance ToJSON SlackPlainTextOnly where
  toJSON (SlackPlainTextOnly (SlackText arr)) =
    object
      [ "type" .= ("plain_text" :: Text)
      , "text" .= intercalate "\n" arr
      ]

instance FromJSON SlackPlainTextOnly where
  parseJSON = withObject "SlackPlainTextOnly" $ \obj -> do
    text <- obj .: "text"
    pure . SlackPlainTextOnly . SlackText $ lines text

-- | Create a 'SlackPlainTextOnly'. Some API points can can take either markdown or plain text,
-- but some can take only plain text. This enforces the latter.
plaintextonly :: (Slack a) => a -> SlackPlainTextOnly
plaintextonly a = SlackPlainTextOnly $ message a

data SlackTextObject
  = SlackPlainText SlackText
  | SlackMarkdownText SlackText
  deriving stock (Eq, Show)

instance ToJSON SlackTextObject where
  toJSON (SlackPlainText (SlackText arr)) =
    object
      [ "type" .= ("plain_text" :: Text)
      , "text" .= intercalate "\n" arr
      ]
  toJSON (SlackMarkdownText (SlackText arr)) =
    object
      [ "type" .= ("mrkdwn" :: Text)
      , "text" .= intercalate "\n" arr
      ]

-- | Create a plain text 'SlackTextObject' where the API allows either markdown or plain text.
plaintext :: (Slack a) => a -> SlackTextObject
plaintext = SlackPlainText . message

-- | Create a markdown 'SlackTextObject' where the API allows either markdown or plain text.
mrkdwn :: (Slack a) => a -> SlackTextObject
mrkdwn = SlackMarkdownText . message

instance FromJSON SlackTextObject where
  parseJSON = withObject "SlackTextObject" $ \obj -> do
    (slackTextType :: Text) <- obj .: "type"
    case slackTextType of
      "plain_text" -> do
        text <- obj .: "text"
        pure . SlackPlainText . SlackText $ lines text
      "mrkdwn" -> do
        text <- obj .: "text"
        pure . SlackMarkdownText . SlackText $ lines text
      _ -> fail $ "Unknown SlackTextObject type " <> show slackTextType <> ", must be one of ['plain_text', 'mrkdwn']"

instance Show SlackText where
  show (SlackText arr) = show $ concat arr

instance ToJSON SlackText where
  toJSON (SlackText arr) = toJSON $ concat arr

instance IsString SlackText where
  fromString = message

instance Slack Text where
  message text = SlackText [text]

instance Slack String where
  message = message @Text . pack

instance Slack Int where
  message = message . show

-- | Represents an optional setting for some Slack Setting.
newtype OptionalSetting a = OptionalSetting {unOptionalSetting :: Maybe a}
  deriving newtype (Eq)

type role OptionalSetting representational

-- | Allows using bare Strings without having to use 'setting'
instance IsString (OptionalSetting String) where
  fromString = OptionalSetting . Just

-- | Allows using bare Texts without having to use 'setting'
instance IsString (OptionalSetting Text) where
  fromString = OptionalSetting . Just . pack

-- | Sets a setting.
setting :: a -> OptionalSetting a
setting = OptionalSetting . Just

-- | Sets the empty setting.
emptySetting :: OptionalSetting a
emptySetting = OptionalSetting Nothing

-- | Styles for Slack [buttons](https://api.slack.com/reference/block-kit/block-elements#button).
-- If no style is given, the default style (black) is used.
data SlackStyle
  = -- | Green button
    SlackStylePrimary
  | -- | Red button
    SlackStyleDanger
  deriving stock (Eq, Show)

$(deriveJSON (jsonDeriveWithAffix "SlackStyle" jsonDeriveOptionsSnakeCase) ''SlackStyle)

-- | Used to identify an action. The ID used should be unique among all actions in the block.
--
--   This is limited to 255 characters, per the Slack documentation at
--   <https://api.slack.com/reference/block-kit/block-elements#button>
newtype SlackActionId = SlackActionId {unSlackActionId :: NonEmptyText 255}
  deriving stock (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

-- FIXME(jadel): SlackActionId might be worth turning into something more type
-- safe: possibly parameterize SlackAction over a sum type parameter

data SlackImage = SlackImage
  { slackImageTitle :: !(Maybe Text)
  , slackImageAltText :: !Text
  -- ^ Optional Title
  , slackImageUrl :: !Text
  }
  deriving stock (Eq)

instance Show SlackImage where
  show (SlackImage _ altText _) = unpack altText

data SlackContent
  = SlackContentText SlackText
  | SlackContentImage SlackImage
  deriving stock (Eq)

slackContentToSlackText :: SlackContent -> Maybe SlackText
slackContentToSlackText c = case c of
  SlackContentText slackText ->
    Just slackText
  SlackContentImage _ ->
    Nothing

instance Show SlackContent where
  show (SlackContentText t) = show t
  show (SlackContentImage i) = show i

instance ToJSON SlackContent where
  toJSON (SlackContentText t) =
    object
      [ "type" .= ("mrkdwn" :: Text)
      , "text" .= t
      ]
  toJSON (SlackContentImage (SlackImage mtitle altText url)) =
    object
      $ [ "type" .= ("image" :: Text)
        , "image_url" .= url
        , "alt_text" .= altText
        ]
      <> maybe [] mkTitle mtitle
    where
      mkTitle title =
        [ "title"
            .= object
              [ "type" .= ("plain_text" :: Text)
              , "text" .= title
              ]
        ]

instance FromJSON SlackContent where
  parseJSON = withObject "SlackContent" $ \obj -> do
    (slackContentType :: Text) <- obj .: "type"
    case slackContentType of
      "mrkdwn" -> do
        (slackContentText :: String) <- obj .: "text"
        pure $ SlackContentText $ fromString slackContentText
      "image" -> do
        (slackImageUrl :: Text) <- obj .: "image_url"
        (slackImageAltText :: Text) <- obj .: "alt_text"
        (slackImageTitleObj :: Maybe Object) <- obj .:? "title"
        (slackImageTitleText :: Maybe Text) <- case slackImageTitleObj of
          Just innerObj -> innerObj .: "text"
          Nothing -> pure Nothing
        pure $ SlackContentImage $ SlackImage slackImageTitleText slackImageAltText slackImageUrl
      _ -> fail $ "Unknown SlackContent type " <> show slackContentType <> ", must be one of ['mrkdwn', 'image']"

newtype SlackContext = SlackContext [SlackContent]
  deriving newtype (Semigroup, Monoid, Eq)

instance Show SlackContext where
  show (SlackContext arr) = show arr

instance ToJSON SlackContext where
  toJSON (SlackContext arr) = toJSON arr

instance FromJSON SlackContext where
  parseJSON = withArray "SlackContext" $ \arr -> do
    (parsedAsArrayOfSlackContents :: V.Vector SlackContent) <- traverse parseJSON arr
    let slackContentList = V.toList parsedAsArrayOfSlackContents
    pure $ SlackContext slackContentList

type SlackActionListConstraints = SizeGreaterThan 0 && SizeLessThan 6

-- | List that enforces that Slack actions must have between 1 and 5 actions.
newtype SlackActionList = SlackActionList {unSlackActionList :: Refined SlackActionListConstraints [SlackAction]}
  deriving stock (Show)
  deriving newtype (Eq, FromJSON, ToJSON)

-- | Helper to allow using up to a 5-tuple for a 'SlackActionList'
class ToSlackActionList a where
  toSlackActionList :: a -> SlackActionList

instance ToSlackActionList SlackActionList where
  toSlackActionList = id

instance ToSlackActionList SlackAction where
  toSlackActionList a = SlackActionList $ reallyUnsafeRefine [a]

instance ToSlackActionList (SlackAction, SlackAction) where
  toSlackActionList (a, b) = SlackActionList $ reallyUnsafeRefine [a, b]

instance ToSlackActionList (SlackAction, SlackAction, SlackAction) where
  toSlackActionList (a, b, c) = SlackActionList $ reallyUnsafeRefine [a, b, c]

instance ToSlackActionList (SlackAction, SlackAction, SlackAction, SlackAction) where
  toSlackActionList (a, b, c, d) = SlackActionList $ reallyUnsafeRefine [a, b, c, d]

instance ToSlackActionList (SlackAction, SlackAction, SlackAction, SlackAction, SlackAction) where
  toSlackActionList (a, b, c, d, e) = SlackActionList $ reallyUnsafeRefine [a, b, c, d, e]

-- | A rich text style. You can't actually send these, for some reason.
data RichStyle = RichStyle
  { rsBold :: Bool
  , rsItalic :: Bool
  }
  deriving stock (Eq, Show)

instance Semigroup RichStyle where
  a <> b = RichStyle {rsBold = rsBold a || rsBold b, rsItalic = rsItalic a || rsItalic b}

instance Monoid RichStyle where
  mempty = RichStyle {rsBold = False, rsItalic = False}

instance FromJSON RichStyle where
  parseJSON = withObject "RichStyle" \obj -> do
    rsBold <- obj .:? "bold" .!= False
    rsItalic <- obj .:? "italic" .!= False
    pure RichStyle {..}

data RichLinkAttrs = RichLinkAttrs
  { style :: RichStyle
  , url :: Text
  , text :: Maybe Text
  -- ^ Probably is empty in the case of links that are just the URL
  }
  deriving stock (Eq, Show)

-- | An inline reference to a message. The URL is optional; consumers can build
-- a permalink from the channel and message timestamp in the current workspace.
--
-- Parsed as part of 'RichItemMessageMention' in incoming rich text.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/message-mention-element/>
--
-- @since 2.3.0.0
data RichMessageMention = RichMessageMention
  { rmmChannelId :: ConversationId
  -- ^ Channel containing the referenced message.
  --
  -- @since 2.3.0.0
  , rmmMessageTs :: Text
  -- ^ Timestamp identifying the referenced message, preserved as Slack's text.
  --
  -- @since 2.3.0.0
  , rmmThreadTs :: Maybe Text
  -- ^ Optional @thread_ts@ timestamp supplied by Slack.
  --
  -- @since 2.3.0.0
  , rmmUrl :: Maybe Text
  -- ^ URL of the referenced message, when supplied by Slack.
  --
  -- @since 2.3.0.0
  }
  deriving stock (Eq, Show)

-- | Inline content inside rich-text sections, quotes, and preformatted blocks.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-section-element/>
--
-- Unrecognized element types are preserved as 'RichItemOther'.
data RichItem
  = RichItemText Text RichStyle
  | RichItemChannel ConversationId
  | RichItemUser UserId RichStyle
  | RichItemLink RichLinkAttrs
  | -- | An inline @message_mention@ reference to another message.
    --
    -- @since 2.3.0.0
    RichItemMessageMention RichMessageMention
  | RichItemEmoji Emoji
  | RichItemOther Text Value
  -- FIXME(jadel): date, usergroup, team, broadcast
  deriving stock (Eq, Show)

instance FromJSON RichItem where
  parseJSON = withObject "RichItem" \obj -> do
    kind :: Text <- obj .: "type"
    case kind of
      "text" -> do
        style <- obj .:? "style" .!= mempty
        text <- obj .: "text"
        pure $ RichItemText text style
      "channel" -> do
        channelId <- obj .: "channel_id"
        pure $ RichItemChannel channelId
      "emoji" -> do
        name <- obj .: "name"
        pure $ RichItemEmoji name
      "link" -> do
        url <- obj .: "url"
        text <- obj .:? "text"
        style <- obj .:? "style" .!= mempty
        pure $ RichItemLink RichLinkAttrs {..}
      "message_mention" -> do
        rmmChannelId <- obj .: "channel_id"
        rmmMessageTs <- obj .: "message_ts"
        rmmThreadTs <- obj .:? "thread_ts"
        rmmUrl <- obj .:? "url"
        pure $ RichItemMessageMention RichMessageMention {..}
      "user" -> do
        userId <- obj .: "user_id"
        style <- obj .:? "style" .!= mempty
        pure $ RichItemUser userId style
      _ -> pure $ RichItemOther kind (Object obj)

-- | A @rich_text_section@ object containing inline 'RichItem' values.
-- A section can appear directly in a @rich_text@ block or as an entry in a
-- @rich_text_list@. Its JSON decoder requires @type = "rich_text_section"@.
--
-- t'RichTextSectionItem' represents the broader set of structural children of
-- a rich-text block; this type represents only a section. Lists contain
-- sections, so their entries use this narrower type.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-section-element/>
--
-- @since 2.3.0.0
newtype RichTextSection = RichTextSection [RichItem]
  deriving stock (Eq, Show)

instance FromJSON RichTextSection where
  parseJSON = withObject "RichTextSection" \obj -> do
    kind :: Text <- obj .: "type"
    case kind of
      "rich_text_section" -> RichTextSection <$> obj .: "elements"
      _ -> fail $ "Unexpected RichTextSection type " <> show kind <> ", must be 'rich_text_section'"

-- | Structural children of a @rich_text@ block: sections, lists, quotes, and
-- preformatted blocks. The name refers to this broader set of containers;
-- t'RichTextSection' represents a single @rich_text_section@, and 'RichItem'
-- represents inline content such as text, links, and message mentions.
--
-- <https://docs.slack.dev/reference/block-kit/blocks/rich-text-block/>
data RichTextSectionItem
  = -- | A @rich_text_section@ containing inline items.
    --
    -- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-section-element/>
    RichTextSectionItemRichText RichTextSection
  | -- | A @rich_text_list@ whose entries are sections. The t'RichTextSection'
    -- decoder rejects entries with any other container type.
    -- The decoder ignores Slack's @style@, @indent@, @offset@, and @border@ fields.
    --
    -- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-list-element/>
    --
    -- @since 2.3.0.0
    RichTextSectionItemList [RichTextSection]
  | -- | A @rich_text_quote@ containing inline items.
    -- The decoder ignores Slack's optional @border@ field.
    --
    -- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-quote-element/>
    --
    -- @since 2.3.0.0
    RichTextSectionItemQuote [RichItem]
  | -- | A @rich_text_preformatted@ code block. Slack documents text and link
    -- elements as its contents; 'RichItem' also permits other constructors.
    -- Border and syntax-highlighting language are not retained.
    --
    -- <https://docs.slack.dev/reference/block-kit/block-elements/rich-text-preformatted-element/>
    --
    -- @since 2.3.0.0
    RichTextSectionItemPreformatted [RichItem]
  | RichTextSectionItemUnknown Text Value
  deriving stock (Eq, Show)

instance FromJSON RichTextSectionItem where
  parseJSON = withObject "RichTextSectionItem" \obj -> do
    kind <- obj .: "type"
    case kind of
      "rich_text_section" -> RichTextSectionItemRichText <$> parseJSON (Object obj)
      "rich_text_list" -> RichTextSectionItemList <$> obj .: "elements"
      "rich_text_quote" -> RichTextSectionItemQuote <$> obj .: "elements"
      "rich_text_preformatted" -> RichTextSectionItemPreformatted <$> obj .: "elements"
      _ -> pure $ RichTextSectionItemUnknown kind (Object obj)

data RichText = RichText
  { blockId :: Maybe SlackBlockId
  , elements :: [RichTextSectionItem]
  }
  deriving stock (Eq, Show)

instance FromJSON RichText where
  parseJSON = withObject "RichText" \obj -> do
    blockId <- obj .:? "block_id"
    elements <- obj .: "elements"
    pure RichText {..}

-- | Accessory is a type of optional block element that floats to the right of text in a BlockSection.
--   <https://api.slack.com/reference/block-kit/blocks#section_fields>
data SlackAccessory
  = SlackButtonAccessory SlackAction -- button
  | -- | An overflow menu beside a section. Construct the action with 'overflow'.
    --
    -- @since 2.4.0.0
    SlackOverflowAccessory SlackAction
  | -- | A static select menu beside a section. Construct the action with 'staticSelect'.
    --
    -- @since 2.4.0.0
    SlackStaticSelectAccessory SlackAction
  | -- | An external select menu beside a section. Construct the action with 'externalSelect'.
    --
    -- @since 2.4.0.0
    SlackExternalSelectAccessory SlackAction
  deriving stock (Eq)

instance ToJSON SlackAccessory where
  toJSON (SlackButtonAccessory btn) = toJSON btn
  toJSON (SlackOverflowAccessory menu) = toJSON menu
  toJSON (SlackStaticSelectAccessory menu) = toJSON menu
  toJSON (SlackExternalSelectAccessory menu) = toJSON menu

instance FromJSON SlackAccessory where
  parseJSON = withObject "SlackAccessory" \obj -> do
    kind :: Text <- obj .: "type"
    case kind of
      "button" -> SlackButtonAccessory <$> parseJSON (Object obj)
      "overflow" -> SlackOverflowAccessory <$> parseJSON (Object obj)
      "static_select" -> SlackStaticSelectAccessory <$> parseJSON (Object obj)
      "external_select" -> SlackExternalSelectAccessory <$> parseJSON (Object obj)
      _ -> fail $ "Unknown SlackAccessory type " <> show kind <> ", must be one of ['button', 'overflow', 'static_select', 'external_select']"

instance Show SlackAccessory where
  show (SlackButtonAccessory btn) = show btn
  show (SlackOverflowAccessory menu) = show menu
  show (SlackStaticSelectAccessory menu) = show menu
  show (SlackExternalSelectAccessory menu) = show menu

-- | Small helper function for constructing a section with a button accessory out of a button and text components
sectionWithButtonAccessory :: SlackAction -> SlackText -> SlackBlock
sectionWithButtonAccessory btn txt =
  SlackBlockSection
    $ (slackSectionWithText txt)
      { slackSectionAccessory = Just $ SlackButtonAccessory btn
      }

-- | <https://api.slack.com/reference/block-kit/blocks#section>
data SlackSection = SlackSection
  { slackSectionText :: Maybe SlackText
  -- ^ May be absent if 'slackSectionFields' is present.
  , slackSectionBlockId :: Maybe SlackBlockId
  , slackSectionFields :: Maybe [SlackText]
  -- ^ Required if 'slackSectionText' is not provided.
  , slackSectionAccessory :: Maybe SlackAccessory
  }
  deriving stock (Eq, Show)

slackSectionWithText :: SlackText -> SlackSection
slackSectionWithText t =
  SlackSection
    { slackSectionText = Just t
    , slackSectionBlockId = Nothing
    , slackSectionFields = Nothing
    , slackSectionAccessory = Nothing
    }

data SlackBlock
  = -- | Section block. Similar in concept to a @p@ or @div@ tag in HTML.
    --
    -- <https://api.slack.com/reference/block-kit/blocks#section>
    SlackBlockSection SlackSection
  | SlackBlockImage SlackImage
  | -- | Context block: smaller, grey, text, and rendered inline. Like a @span@ in HTML.
    --
    -- <https://api.slack.com/reference/block-kit/blocks#context>
    SlackBlockContext SlackContext
  | -- | Horizontal line. Similar to an html @hr@ tag.
    --
    -- <https://api.slack.com/reference/block-kit/blocks#divider>
    SlackBlockDivider
  | SlackBlockRichText RichText
  | -- | Inline container for interactive actions.
    --
    -- <https://api.slack.com/reference/block-kit/blocks#actions>
    SlackBlockActions (Maybe SlackBlockId) SlackActionList -- 1 to 5 elements
  | -- | Header block.
    --
    -- The text is max 150 characters long.
    --
    -- <https://api.slack.com/reference/block-kit/blocks#header>
    SlackBlockHeader SlackPlainTextOnly
  | SlackBlockOther Object
  deriving stock (Eq)

instance Show SlackBlock where
  show (SlackBlockSection section) = show section
  show (SlackBlockImage i) = show i
  show (SlackBlockContext contents) = show contents
  show SlackBlockDivider = "|"
  show (SlackBlockActions mBlockId as) =
    show
      $ mconcat
        [ "actions("
        , show mBlockId
        , ") = ["
        , show $ intercalate ", " (map show (unrefine $ unSlackActionList as))
        , "]"
        ]
  show (SlackBlockRichText rt) = show rt
  show (SlackBlockHeader p) = show p
  show (SlackBlockOther o) = "SlackBlockOther " <> show o

instance ToJSON SlackBlock where
  toJSON (SlackBlockSection SlackSection {..}) =
    objectOptional
      [ "type" .=! ("section" :: Text)
      , "text" .=? (SlackContentText <$> slackSectionText)
      , "block_id" .=? slackSectionBlockId
      , "fields" .=? (map SlackContentText <$> slackSectionFields)
      , "accessory" .=? slackSectionAccessory
      ]
  toJSON (SlackBlockImage i) = toJSON (SlackContentImage i)
  toJSON (SlackBlockContext contents) =
    object
      [ "type" .= ("context" :: Text)
      , "elements" .= contents
      ]
  toJSON SlackBlockDivider =
    object
      [ "type" .= ("divider" :: Text)
      ]
  toJSON (SlackBlockActions mBlockId as) =
    objectOptional
      [ "type" .=! ("actions" :: Text)
      , "block_id" .=? mBlockId
      , "elements" .=! as
      ]
  -- FIXME(jadel): should this be an error? Slack doesn't accept these
  toJSON (SlackBlockRichText _) =
    object []
  toJSON (SlackBlockHeader slackPlainText) =
    object
      [ "type" .= ("header" :: Text)
      , "text" .= slackPlainText
      ]
  toJSON (SlackBlockOther o) = Object o

instance FromJSON SlackBlock where
  parseJSON = withObject "SlackBlock" $ \obj -> do
    (slackBlockType :: Text) <- obj .: "type"
    case slackBlockType of
      "section" -> do
        slackSectionTextContent <- obj .:? "text"
        let slackSectionText = slackSectionTextContent >>= slackContentToSlackText
        slackSectionBlockId <- obj .:? "block_id"
        slackSectionFieldsContent <- obj .:? "fields"
        let slackSectionFields = slackSectionFieldsContent >>= traverse slackContentToSlackText
        (slackSectionAccessoryValue :: Maybe Value) <- obj .:? "accessory"
        -- The section accessory can be any block element but `SlackAcessory`
        -- only implements button.
        let slackSectionAccessory =
              slackSectionAccessoryValue >>= \v ->
                case fromJSON v of
                  Error _ ->
                    Nothing
                  Success slackAccessory ->
                    Just slackAccessory
        pure $ SlackBlockSection SlackSection {..}
      "context" -> do
        slackContent <- obj .: "elements"
        pure $ SlackBlockContext slackContent
      "image" -> do
        SlackContentImage i <- parseJSON $ Object obj
        pure $ SlackBlockImage i
      "divider" -> pure SlackBlockDivider
      "actions" -> do
        slackActions <- obj .: "elements"
        mBlockId <- obj .:? "block_id"
        pure $ SlackBlockActions mBlockId slackActions
      "rich_text" -> do
        elements <- obj .: "elements"
        mBlockId <- obj .:? "block_id"
        pure
          . SlackBlockRichText
          $ RichText
            { blockId = mBlockId
            , elements
            }
      "header" -> do
        (headerContentObj :: Value) <- obj .: "text"
        headerContentText <- parseJSON headerContentObj
        pure $ SlackBlockHeader headerContentText
      _unk -> pure $ SlackBlockOther obj

newtype SlackMessage = SlackMessage [SlackBlock]
  deriving newtype (Semigroup, Monoid, Eq)

instance Show SlackMessage where
  show (SlackMessage arr) = intercalate " " (map show arr)

instance ToJSON SlackMessage where
  toJSON (SlackMessage arr) = toJSON arr

instance FromJSON SlackMessage where
  parseJSON = withArray "SlackMessage" $ \arr -> do
    (parsedAsArrayOfSlackBlocks :: V.Vector SlackBlock) <- traverse parseJSON arr
    let slackBlockList = V.toList parsedAsArrayOfSlackBlocks
    pure $ SlackMessage slackBlockList

textToMessage :: Text -> SlackMessage
textToMessage = markdown . message

class Markdown a where
  markdown :: SlackText -> a

instance Markdown SlackMessage where
  markdown t = SlackMessage [SlackBlockSection (slackSectionWithText t)]

instance Markdown SlackContext where
  markdown t = SlackContext [SlackContentText t]

class Image a where
  image :: SlackImage -> a

instance Image SlackMessage where
  image i = SlackMessage [SlackBlockImage i]

instance Image SlackContext where
  image i = SlackContext [SlackContentImage i]

context :: SlackContext -> SlackMessage
context c = SlackMessage [SlackBlockContext c]

textToContext :: Text -> SlackMessage
textToContext = context . markdown . message

-- | Generates interactive components such as buttons.
actions :: (ToSlackActionList as) => as -> SlackMessage
actions as = SlackMessage [SlackBlockActions Nothing $ toSlackActionList as]

-- | Generates interactive components such as buttons with a 'SlackBlockId'.
actionsWithBlockId :: (ToSlackActionList as) => SlackBlockId -> as -> SlackMessage
actionsWithBlockId slackBlockId as = SlackMessage [SlackBlockActions (Just slackBlockId) $ toSlackActionList as]

-- | Settings for [button elements](https://api.slack.com/reference/block-kit/block-elements#button).
data ButtonSettings = ButtonSettings
  { buttonUrl :: OptionalSetting (NonEmptyText 3000)
  -- ^ Optional URL to load into the user's browser.
  -- However, Slack will still call the webhook and you must send an acknowledgement response.
  , buttonValue :: OptionalSetting (NonEmptyText 2000)
  -- ^ Optional value to send with the interaction payload.
  -- One commoon use is to send state via JSON encoding.
  , buttonStyle :: OptionalSetting SlackStyle
  -- ^ Optional 'SlackStyle'. If not provided, uses the default style which is a black button.
  , buttonConfirm :: OptionalSetting SlackConfirmObject
  -- ^ An optional confirmation dialog to display.
  }

-- | Default button settings.
buttonSettings :: ButtonSettings
buttonSettings =
  ButtonSettings
    { buttonUrl = emptySetting
    , buttonValue = emptySetting
    , buttonStyle = emptySetting
    , buttonConfirm = emptySetting
    }

-- | Button builder.
button :: SlackActionId -> SlackButtonText -> ButtonSettings -> SlackAction
button actionId buttonText ButtonSettings {..} =
  SlackAction (Just actionId)
    $ SlackButton
    $ SlackButtonElement
      { slackButtonText = buttonText
      , slackButtonUrl = unOptionalSetting buttonUrl
      , slackButtonValue = unOptionalSetting buttonValue
      , slackButtonStyle = unOptionalSetting buttonStyle
      , slackButtonConfirm = unOptionalSetting buttonConfirm
      }

-- | A divider block.
-- https://api.slack.com/reference/block-kit/blocks#divider
--
-- @since 1.6.2.0
divider :: SlackMessage
divider = SlackMessage [SlackBlockDivider]

-- | Settings for [confirmation dialog objects](https://api.slack.com/reference/block-kit/composition-objects#confirm).
data ConfirmSettings = ConfirmSettings
  { confirmTitle :: Text
  -- ^ Plain text title for the dialog window. Max length 100 characters.
  , confirmText :: Text
  -- ^ Markdown explanatory text that appears in the confirm dialog.
  -- Max length is 300 characters.
  , confirmConfirm :: Text
  -- ^ Plain text to display in the \"confirm\" button.
  -- Max length is 30 characters.
  , confirmDeny :: Text
  -- ^ Plain text to display in the \"deny\" button.
  -- Max length is 30 characters.
  , confirmStyle :: OptionalSetting SlackStyle
  -- ^ Optional 'SlackStyle' to use for the \"confirm\" button.
  }

-- | Default settings for a \"Are you sure?\" confirmation dialog.
confirmAreYouSure :: ConfirmSettings
confirmAreYouSure =
  ConfirmSettings
    { confirmTitle = "Are You Sure?"
    , confirmText = "Are you sure you wish to perform this operation?"
    , confirmConfirm = "Yes"
    , confirmDeny = "No"
    , confirmStyle = emptySetting
    }

-- | Confirm dialog builder.
confirm :: ConfirmSettings -> SlackConfirmObject
confirm ConfirmSettings {..} =
  SlackConfirmObject
    { slackConfirmTitle = plaintextonly confirmTitle
    , slackConfirmText = mrkdwn confirmText
    , slackConfirmConfirm = plaintextonly confirmConfirm
    , slackConfirmDeny = plaintextonly confirmDeny
    , slackConfirmStyle = unOptionalSetting confirmStyle
    }

-- | 'SlackBlockId' should be unique for each message and each iteration
-- of a message. If a message is updated, use a new block_id.
type SlackBlockId = NonEmptyText 255

-- | A component with an optional action identifier. Slack may omit @action_id@
-- in message blocks.
data SlackAction = SlackAction
  { slackActionId :: Maybe SlackActionId
  -- ^ Optional identifier for this action, unique within its block.
  --
  -- @since 2.4.0.0
  , slackActionComponent :: SlackActionComponent
  -- ^ Interactive component associated with the optional identifier.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq)

instance Show SlackAction where
  show SlackAction {..} = maybe "" (\actionId -> show actionId <> " ") slackActionId <> show slackActionComponent

-- | [Confirm dialog object](https://api.slack.com/reference/block-kit/composition-objects#confirm).
data SlackConfirmObject = SlackConfirmObject
  { slackConfirmTitle :: SlackPlainTextOnly -- max length 100
  , slackConfirmText :: SlackTextObject -- max length 300
  , slackConfirmConfirm :: SlackPlainTextOnly -- max length 30
  , slackConfirmDeny :: SlackPlainTextOnly -- max length 30
  , slackConfirmStyle :: Maybe SlackStyle
  }
  deriving stock (Eq, Show)

instance ToJSON SlackConfirmObject where
  toJSON SlackConfirmObject {..} =
    objectOptional
      [ "title" .=! slackConfirmTitle
      , "text" .=! slackConfirmText
      , "confirm" .=! slackConfirmConfirm
      , "deny" .=! slackConfirmDeny
      , "style" .=? slackConfirmStyle
      ]

instance FromJSON SlackConfirmObject where
  parseJSON = withObject "SlackConfirmObject" $ \obj -> do
    slackConfirmTitle <- obj .: "title"
    slackConfirmText <- obj .: "text"
    slackConfirmConfirm <- obj .: "confirm"
    slackConfirmDeny <- obj .: "deny"
    slackConfirmStyle <- obj .:? "style"
    pure SlackConfirmObject {..}

newtype SlackResponseUrl = SlackResponseUrl {unSlackResponseUrl :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

-- | Represents the data we get from a callback from Slack for interactive
-- operations. See https://api.slack.com/interactivity/handling#payloads
data SlackInteractivePayload = SlackInteractivePayload
  { sipUserId :: Text
  , sipUsername :: Text
  , sipName :: Text
  , sipResponseUrl :: Maybe SlackResponseUrl
  , sipTriggerId :: Maybe Text
  , sipActions :: [SlackActionResponse]
  }
  deriving stock (Show)

instance FromJSON SlackInteractivePayload where
  parseJSON = withObject "SlackInteractivePayload" $ \obj -> do
    user <- obj .: "user"
    sipUserId <- user .: "id"
    sipUsername <- user .: "username"
    sipName <- user .: "name"
    sipResponseUrl <- obj .:? "response_url"
    sipTriggerId <- obj .:? "trigger_id"
    actionsObj <- obj .: "actions"
    sipActions <- parseJSON actionsObj
    pure $ SlackInteractivePayload {..}

-- | Which component and it's IDs that triggered an interactive webhook call.
data SlackActionResponse = SlackActionResponse
  { sarBlockId :: SlackBlockId
  , sarActionId :: SlackActionId
  , sarActionComponent :: SlackActionComponent
  }
  deriving stock (Show)

instance FromJSON SlackActionResponse where
  parseJSON = withObject "SlackActionResponse" $ \obj -> do
    sarBlockId <- obj .: "block_id"
    sarActionId <- obj .: "action_id"
    sarActionComponent <- parseJSON $ Object obj
    pure $ SlackActionResponse {..}

data SlackInteractiveResponseResponse = SlackInteractiveResponseResponse {unSlackInteractiveResponseResponse :: Bool}

instance FromJSON SlackInteractiveResponseResponse where
  parseJSON = withObject "SlackInteractiveResponseResponse" $ \obj -> do
    res <- obj .: "ok"
    pure $ SlackInteractiveResponseResponse res

-- | Type of message to send in response to an interactive webhook.
-- See Slack's [Handling user interaction in your Slack apps](https://api.slack.com/interactivity/handling#responses)
-- for a description of these fieldds.
data SlackInteractiveResponse
  = -- | Respond with a new message.
    SlackInteractiveResponse SlackMessage
  | -- | Respond with a message that only the interacting user can usee.
    Ephemeral SlackMessage
  | -- | Replace the original message.
    ReplaceOriginal SlackMessage
  | -- | Delete the original message.
    DeleteOriginal
  deriving stock (Show)

instance ToJSON SlackInteractiveResponse where
  toJSON (SlackInteractiveResponse msg) = object ["blocks" .= msg]
  toJSON (Ephemeral msg) = object ["blocks" .= msg, "replace_original" .= False, "response_type" .= ("ephemeral" :: Text)]
  toJSON (ReplaceOriginal msg) = object ["blocks" .= msg, "replace_original" .= True]
  toJSON DeleteOriginal = object ["delete_original" .= True]

-- | Text to be displayed in a 'SlackButton'.
-- Up to 75 characters, but may be truncated to 30 characters.
newtype SlackButtonText = SlackButtonText (NonEmptyText 75)
  deriving stock (Show)
  deriving newtype (Eq, FromJSON)

instance Slack SlackButtonText where
  message (SlackButtonText m) = SlackText [nonEmptyTextToText m]

-- It isn't the end of the world if this gets truncated (slack may truncate it
-- to about 30 characters anyway) so we have this convenience instance to
-- use plain strings for button text.
instance IsString SlackButtonText where
  fromString s = SlackButtonText . unsafeMkNonEmptyText . cs $ take 75 s

-- | An option in an overflow menu. Text and description must be plain text
-- (up to 75 characters); the value may contain up to 150 characters.
--
-- <https://docs.slack.dev/reference/block-kit/composition-objects/option-object/>
--
-- @since 2.4.0.0
data SlackOverflowOption = SlackOverflowOption
  { slackOverflowOptionText :: SlackPlainTextOnly
  -- ^ Label displayed for this option. Slack allows up to 75 characters;
  -- this limit is not enforced by the type.
  --
  -- @since 2.4.0.0
  , slackOverflowOptionValue :: Text
  -- ^ Value identifying this option in an interaction payload. Slack allows
  -- up to 150 characters; this limit is not enforced by the type.
  --
  -- @since 2.4.0.0
  , slackOverflowOptionDescription :: Maybe SlackPlainTextOnly
  -- ^ Optional description displayed below the label. Slack allows up to
  -- 75 characters; this limit is not enforced by the type.
  --
  -- @since 2.4.0.0
  , slackOverflowOptionUrl :: Maybe (NonEmptyText 3000)
  -- ^ Optional URL opened when the option is selected, up to 3000 characters.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

instance FromJSON SlackOverflowOption where
  parseJSON = withObject "SlackOverflowOption" $ \obj -> do
    slackOverflowOptionText <- obj .: "text"
    slackOverflowOptionValue <- obj .: "value"
    slackOverflowOptionDescription <- obj .:? "description"
    slackOverflowOptionUrl <- obj .:? "url"
    pure SlackOverflowOption {..}

instance ToJSON SlackOverflowOption where
  toJSON SlackOverflowOption {..} =
    objectOptional
      [ "text" .=! slackOverflowOptionText
      , "value" .=! slackOverflowOptionValue
      , "description" .=? slackOverflowOptionDescription
      , "url" .=? slackOverflowOptionUrl
      ]

-- | Overflow menus contain between one and five options.
-- Construct the refined list with 'refine'; JSON decoding also checks its size.
--
-- @since 2.4.0.0
newtype SlackOverflowOptions = SlackOverflowOptions
  { unSlackOverflowOptions :: Refined (SizeGreaterThan 0 && SizeLessThan 6) [SlackOverflowOption]
  -- ^ The nonempty list of at most five menu options.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Show)
  deriving newtype (Eq, FromJSON, ToJSON)

-- | Build an overflow menu with an optional confirmation dialog.
-- Use the result in an actions block or wrap it in 'SlackOverflowAccessory'
-- for a section. This builder supplies an action ID; to represent a message
-- where Slack omitted it, use
-- @SlackAction Nothing (SlackOverflow (SlackOverflowMenu options confirmDialog))@.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/overflow-menu-element/>
--
-- @since 2.4.0.0
overflow :: SlackActionId -> SlackOverflowOptions -> OverflowSettings -> SlackAction
overflow actionId options OverflowSettings {..} =
  SlackAction (Just actionId) $ SlackOverflow $ SlackOverflowMenu options (unOptionalSetting overflowConfirm)

-- | Optional settings for 'overflow'.
--
-- @since 2.4.0.0
newtype OverflowSettings = OverflowSettings
  { overflowConfirm :: OptionalSetting SlackConfirmObject
  -- ^ Optional confirmation dialog.
  --
  -- @since 2.4.0.0
  }

-- | Default overflow settings, omitting the confirmation dialog.
--
-- @since 2.4.0.0
overflowSettings :: OverflowSettings
overflowSettings = OverflowSettings {overflowConfirm = emptySetting}

-- | A select-menu option. Unlike overflow options, select options cannot have
-- a URL. Text and description are plain text (up to 75 characters); the value
-- may contain up to 150 characters. These text limits are not enforced by the type.
--
-- <https://docs.slack.dev/reference/block-kit/composition-objects/option-object/>
--
-- @since 2.4.0.0
data SlackSelectOption = SlackSelectOption
  { slackSelectOptionText :: SlackPlainTextOnly
  -- ^ Label displayed for the option.
  --
  -- @since 2.4.0.0
  , slackSelectOptionValue :: Text
  -- ^ Value identifying this option in an interaction payload.
  --
  -- @since 2.4.0.0
  , slackSelectOptionDescription :: Maybe SlackPlainTextOnly
  -- ^ Optional description displayed with the label.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

instance FromJSON SlackSelectOption where
  parseJSON = withObject "SlackSelectOption" $ \obj -> do
    slackSelectOptionText <- obj .: "text"
    slackSelectOptionValue <- obj .: "value"
    slackSelectOptionDescription <- obj .:? "description"
    pure SlackSelectOption {..}

instance ToJSON SlackSelectOption where
  toJSON SlackSelectOption {..} =
    objectOptional
      [ "text" .=! slackSelectOptionText
      , "value" .=! slackSelectOptionValue
      , "description" .=? slackSelectOptionDescription
      ]

-- | A labelled group of up to 100 select-menu options.
--
-- <https://docs.slack.dev/reference/block-kit/composition-objects/option-group-object/>
--
-- @since 2.4.0.0
data SlackSelectOptionGroup = SlackSelectOptionGroup
  { slackSelectOptionGroupLabel :: SlackPlainTextOnly
  -- ^ Plain-text heading displayed above this group's options.
  --
  -- @since 2.4.0.0
  , slackSelectOptionGroupOptions :: Refined (SizeLessThan 101) [SlackSelectOption]
  -- ^ Options in this group. Construct the bounded list with 'refine'; JSON
  -- decoding also checks the maximum of 100 options.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

instance FromJSON SlackSelectOptionGroup where
  parseJSON = withObject "SlackSelectOptionGroup" $ \obj -> do
    slackSelectOptionGroupLabel <- obj .: "label"
    slackSelectOptionGroupOptions <- obj .: "options"
    pure SlackSelectOptionGroup {..}

instance ToJSON SlackSelectOptionGroup where
  toJSON SlackSelectOptionGroup {..} =
    object
      [ "label" .= slackSelectOptionGroupLabel
      , "options" .= slackSelectOptionGroupOptions
      ]

-- | A static menu supplies either options or option groups, never both.
-- JSON decoding rejects objects with both sources or neither source.
--
-- @since 2.4.0.0
data SlackStaticSelectOptions
  = -- | A flat list of at most 100 options, bounded using 'refine'.
    --
    -- @since 2.4.0.0
    SlackSelectOptions (Refined (SizeLessThan 101) [SlackSelectOption])
  | -- | At most 100 labelled groups, bounded using 'refine'.
    --
    -- @since 2.4.0.0
    SlackSelectOptionGroups (Refined (SizeLessThan 101) [SlackSelectOptionGroup])
  deriving stock (Eq, Show)

instance FromJSON SlackStaticSelectOptions where
  parseJSON = withObject "SlackStaticSelectOptions" $ \obj -> do
    options <- obj .:? "options"
    groups <- obj .:? "option_groups"
    case (options, groups) of
      (Just opts, Nothing) -> pure $ SlackSelectOptions opts
      (Nothing, Just grps) -> pure $ SlackSelectOptionGroups grps
      _ -> fail "A static select menu requires exactly one of 'options' or 'option_groups'"

-- | Encode the option source as an @options@ or @option_groups@ field for
-- inclusion in a static select menu's JSON object.
--
-- @since 2.4.0.0
slackStaticSelectOptionsPair :: SlackStaticSelectOptions -> Pair
slackStaticSelectOptionsPair = \case
  SlackSelectOptions options -> "options" .= options
  SlackSelectOptionGroups groups -> "option_groups" .= groups

-- | The definition of a static select menu in a message block. This is not
-- the @selected_option@ sent in an interaction response.
-- Use 'staticSelect' to build one for sending in a message.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element/>
--
-- @since 2.4.0.0
data SlackStaticSelectMenu = SlackStaticSelectMenu
  { slackStaticSelectOptions :: SlackStaticSelectOptions
  -- ^ Available options, either flat or grouped.
  --
  -- @since 2.4.0.0
  , slackStaticSelectInitialOption :: Maybe SlackSelectOption
  -- ^ Optional selection displayed when the menu first loads.
  --
  -- @since 2.4.0.0
  , slackStaticSelectConfirm :: Maybe SlackConfirmObject
  -- ^ Optional confirmation dialog for a selection.
  --
  -- @since 2.4.0.0
  , slackStaticSelectFocusOnLoad :: Maybe Bool
  -- ^ Whether to focus this menu when its containing view opens.
  -- 'Nothing' omits the field from JSON.
  --
  -- @since 2.4.0.0
  , slackStaticSelectPlaceholder :: Maybe SlackPlainTextOnly
  -- ^ Optional prompt displayed before an option is selected.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

instance FromJSON SlackStaticSelectMenu where
  parseJSON = withObject "SlackStaticSelectMenu" $ \obj -> do
    slackStaticSelectOptions <- parseJSON $ Object obj
    slackStaticSelectInitialOption <- obj .:? "initial_option"
    slackStaticSelectConfirm <- obj .:? "confirm"
    slackStaticSelectFocusOnLoad <- obj .:? "focus_on_load"
    slackStaticSelectPlaceholder <- obj .:? "placeholder"
    pure SlackStaticSelectMenu {..}

-- | The definition of an external select menu in a message block. Its options
-- are loaded separately, so no options list is required here.
-- Use 'externalSelect' to build one for sending in a message.
--
-- <https://docs.slack.dev/reference/block-kit/block-elements/select-menu-element/>
--
-- @since 2.4.0.0
data SlackExternalSelectMenu = SlackExternalSelectMenu
  { slackExternalSelectInitialOption :: Maybe SlackSelectOption
  -- ^ Optional selection displayed when the menu first loads.
  --
  -- @since 2.4.0.0
  , slackExternalSelectMinQueryLength :: Maybe Natural
  -- ^ Minimum query length before loading options. Zero is permitted;
  -- 'Nothing' omits the field so Slack uses its default.
  --
  -- @since 2.4.0.0
  , slackExternalSelectConfirm :: Maybe SlackConfirmObject
  -- ^ Optional confirmation dialog for a selection.
  --
  -- @since 2.4.0.0
  , slackExternalSelectFocusOnLoad :: Maybe Bool
  -- ^ Whether to focus this menu when its containing view opens.
  -- 'Nothing' omits the field from JSON.
  --
  -- @since 2.4.0.0
  , slackExternalSelectPlaceholder :: Maybe SlackPlainTextOnly
  -- ^ Optional prompt displayed before an option is selected.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

instance FromJSON SlackExternalSelectMenu where
  parseJSON = withObject "SlackExternalSelectMenu" $ \obj -> do
    slackExternalSelectInitialOption <- obj .:? "initial_option"
    slackExternalSelectMinQueryLength <- obj .:? "min_query_length"
    slackExternalSelectConfirm <- obj .:? "confirm"
    slackExternalSelectFocusOnLoad <- obj .:? "focus_on_load"
    slackExternalSelectPlaceholder <- obj .:? "placeholder"
    pure SlackExternalSelectMenu {..}

-- | Optional settings for 'staticSelect'. See 'SlackStaticSelectMenu' for field semantics.
--
-- @since 2.4.0.0
data StaticSelectSettings = StaticSelectSettings
  { staticSelectInitialOption :: OptionalSetting SlackSelectOption
  -- ^ See 'slackStaticSelectInitialOption'.
  --
  -- @since 2.4.0.0
  , staticSelectConfirm :: OptionalSetting SlackConfirmObject
  -- ^ See 'slackStaticSelectConfirm'.
  --
  -- @since 2.4.0.0
  , staticSelectFocusOnLoad :: OptionalSetting Bool
  -- ^ See 'slackStaticSelectFocusOnLoad'.
  --
  -- @since 2.4.0.0
  , staticSelectPlaceholder :: OptionalSetting SlackPlainTextOnly
  -- ^ See 'slackStaticSelectPlaceholder'.
  --
  -- @since 2.4.0.0
  }

-- | Default static select settings, omitting all optional fields.
--
-- @since 2.4.0.0
staticSelectSettings :: StaticSelectSettings
staticSelectSettings =
  StaticSelectSettings
    { staticSelectInitialOption = emptySetting
    , staticSelectConfirm = emptySetting
    , staticSelectFocusOnLoad = emptySetting
    , staticSelectPlaceholder = emptySetting
    }

-- | Build a static select menu with an action ID and flat or grouped options.
--
-- @since 2.4.0.0
staticSelect :: SlackActionId -> SlackStaticSelectOptions -> StaticSelectSettings -> SlackAction
staticSelect actionId options StaticSelectSettings {..} =
  SlackAction (Just actionId)
    $ SlackStaticSelect
      SlackStaticSelectMenu
        { slackStaticSelectOptions = options
        , slackStaticSelectInitialOption = unOptionalSetting staticSelectInitialOption
        , slackStaticSelectConfirm = unOptionalSetting staticSelectConfirm
        , slackStaticSelectFocusOnLoad = unOptionalSetting staticSelectFocusOnLoad
        , slackStaticSelectPlaceholder = unOptionalSetting staticSelectPlaceholder
        }

-- | Optional settings for 'externalSelect'. See 'SlackExternalSelectMenu' for field semantics.
--
-- @since 2.4.0.0
data ExternalSelectSettings = ExternalSelectSettings
  { externalSelectInitialOption :: OptionalSetting SlackSelectOption
  -- ^ See 'slackExternalSelectInitialOption'.
  --
  -- @since 2.4.0.0
  , externalSelectMinQueryLength :: OptionalSetting Natural
  -- ^ See 'slackExternalSelectMinQueryLength'.
  --
  -- @since 2.4.0.0
  , externalSelectConfirm :: OptionalSetting SlackConfirmObject
  -- ^ See 'slackExternalSelectConfirm'.
  --
  -- @since 2.4.0.0
  , externalSelectFocusOnLoad :: OptionalSetting Bool
  -- ^ See 'slackExternalSelectFocusOnLoad'.
  --
  -- @since 2.4.0.0
  , externalSelectPlaceholder :: OptionalSetting SlackPlainTextOnly
  -- ^ See 'slackExternalSelectPlaceholder'.
  --
  -- @since 2.4.0.0
  }

-- | Default external select settings, omitting all optional fields.
--
-- @since 2.4.0.0
externalSelectSettings :: ExternalSelectSettings
externalSelectSettings =
  ExternalSelectSettings
    { externalSelectInitialOption = emptySetting
    , externalSelectMinQueryLength = emptySetting
    , externalSelectConfirm = emptySetting
    , externalSelectFocusOnLoad = emptySetting
    , externalSelectPlaceholder = emptySetting
    }

-- | Build an external select menu with an action ID.
--
-- @since 2.4.0.0
externalSelect :: SlackActionId -> ExternalSelectSettings -> SlackAction
externalSelect actionId ExternalSelectSettings {..} =
  SlackAction (Just actionId)
    $ SlackExternalSelect
      SlackExternalSelectMenu
        { slackExternalSelectInitialOption = unOptionalSetting externalSelectInitialOption
        , slackExternalSelectMinQueryLength = unOptionalSetting externalSelectMinQueryLength
        , slackExternalSelectConfirm = unOptionalSetting externalSelectConfirm
        , slackExternalSelectFocusOnLoad = unOptionalSetting externalSelectFocusOnLoad
        , slackExternalSelectPlaceholder = unOptionalSetting externalSelectPlaceholder
        }

-- | A button's text and optional action settings.
--
-- @since 2.4.0.0
data SlackButtonElement = SlackButtonElement
  { slackButtonText :: SlackButtonText -- max length 75, may truncate to ~30
  , slackButtonUrl :: Maybe (NonEmptyText 3000) -- max length 3000
  , slackButtonValue :: Maybe (NonEmptyText 2000) -- max length 2000
  , slackButtonStyle :: Maybe SlackStyle
  , slackButtonConfirm :: Maybe SlackConfirmObject
  }
  deriving stock (Eq, Show)

-- | An overflow menu with one to five options and an optional confirmation dialog.
--
-- @since 2.4.0.0
data SlackOverflowMenu = SlackOverflowMenu
  { slackOverflowOptions :: SlackOverflowOptions
  -- ^ Options displayed in the menu.
  --
  -- @since 2.4.0.0
  , slackOverflowConfirm :: Maybe SlackConfirmObject
  -- ^ Optional dialog shown before completing the selected action.
  --
  -- @since 2.4.0.0
  }
  deriving stock (Eq, Show)

-- | A component in a message's 'SlackAction'. Use builder functions such as
-- 'button', 'overflow', 'staticSelect', and 'externalSelect'.
data SlackActionComponent
  = SlackButton SlackButtonElement
  | -- | An overflow menu in a message, with one to five options.
    --
    -- @since 2.4.0.0
    SlackOverflow SlackOverflowMenu
  | -- | A select menu whose options are included in the message.
    --
    -- @since 2.4.0.0
    SlackStaticSelect SlackStaticSelectMenu
  | -- | A select menu whose options are loaded from an external source.
    --
    -- @since 2.4.0.0
    SlackExternalSelect SlackExternalSelectMenu
  deriving stock (Eq)

instance FromJSON SlackActionComponent where
  parseJSON = withObject "SlackActionComponent" $ \obj -> do
    (slackActionType :: Text) <- obj .: "type"
    case slackActionType of
      "button" -> do
        text <- obj .: "text"
        slackButtonText <- text .: "text"
        slackButtonUrl <- obj .:? "url"
        slackButtonValue <- obj .:? "value"
        slackButtonStyle <- obj .:? "style"
        slackButtonConfirm <- obj .:? "confirm"
        pure $ SlackButton SlackButtonElement {..}
      "overflow" -> do
        slackOverflowOptions <- obj .: "options"
        slackOverflowConfirm <- obj .:? "confirm"
        pure $ SlackOverflow SlackOverflowMenu {..}
      "static_select" -> SlackStaticSelect <$> parseJSON (Object obj)
      "external_select" -> SlackExternalSelect <$> parseJSON (Object obj)
      _ -> fail $ "Unknown SlackActionComponent type " <> show slackActionType <> ", must be one of ['button', 'overflow', 'static_select', 'external_select']"

instance Show SlackActionComponent where
  show (SlackButton SlackButtonElement {..}) = "[button " <> show slackButtonText <> "]"
  show (SlackOverflow SlackOverflowMenu {..}) = "[overflow " <> show slackOverflowOptions <> " " <> show slackOverflowConfirm <> "]"
  show (SlackStaticSelect menu) = "[static_select " <> show menu <> "]"
  show (SlackExternalSelect menu) = "[external_select " <> show menu <> "]"

instance ToJSON SlackAction where
  toJSON SlackAction {..} = slackActionJSON slackActionId slackActionComponent

-- | Encode an action component, omitting @action_id@ when the identifier is
-- 'Nothing'. This is also the encoding used by the 'ToJSON' instance for
-- 'SlackAction'.
--
-- @since 2.4.0.0
slackActionJSON :: Maybe SlackActionId -> SlackActionComponent -> Value
slackActionJSON actionId = \case
  SlackButton SlackButtonElement {..} ->
    objectOptional
      [ "type" .=! ("button" :: Text)
      , "action_id" .=? actionId
      , "text" .=! plaintext slackButtonText
      , "url" .=? slackButtonUrl
      , "value" .=? slackButtonValue
      , "style" .=? slackButtonStyle
      , "confirm" .=? slackButtonConfirm
      ]
  SlackOverflow SlackOverflowMenu {..} ->
    objectOptional
      [ "type" .=! ("overflow" :: Text)
      , "action_id" .=? actionId
      , "options" .=! slackOverflowOptions
      , "confirm" .=? slackOverflowConfirm
      ]
  SlackStaticSelect SlackStaticSelectMenu {..} ->
    objectOptional
      [ "type" .=! ("static_select" :: Text)
      , "action_id" .=? actionId
      , Just $ slackStaticSelectOptionsPair slackStaticSelectOptions
      , "initial_option" .=? slackStaticSelectInitialOption
      , "confirm" .=? slackStaticSelectConfirm
      , "focus_on_load" .=? slackStaticSelectFocusOnLoad
      , "placeholder" .=? slackStaticSelectPlaceholder
      ]
  SlackExternalSelect SlackExternalSelectMenu {..} ->
    objectOptional
      [ "type" .=! ("external_select" :: Text)
      , "action_id" .=? actionId
      , "initial_option" .=? slackExternalSelectInitialOption
      , "min_query_length" .=? slackExternalSelectMinQueryLength
      , "confirm" .=? slackExternalSelectConfirm
      , "focus_on_load" .=? slackExternalSelectFocusOnLoad
      , "placeholder" .=? slackExternalSelectPlaceholder
      ]

instance FromJSON SlackAction where
  parseJSON = withObject "SlackAction" $ \obj -> do
    slackActionId <- obj .:? "action_id"
    slackActionComponent <- parseJSON $ Object obj
    pure SlackAction {..}
