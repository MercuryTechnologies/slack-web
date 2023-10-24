{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Slack.Experimental.Blocks.Types where

import Control.Monad (MonadFail (..))
import Data.Aeson (Object, Value (..), withArray)
import Data.Aeson.Types ((.!=))
import Data.StringVariants
import Data.Vector qualified as V
import Refined
import Refined.Unsafe (reallyUnsafeRefine)
import Web.Slack.AesonUtils
import Web.Slack.Common (ConversationId, UserId)
import Web.Slack.Prelude

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
plaintextonly :: Slack a => a -> SlackPlainTextOnly
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
plaintext :: Slack a => a -> SlackTextObject
plaintext = SlackPlainText . message

-- | Create a markdown 'SlackTextObject' where the API allows either markdown or plain text.
mrkdwn :: Slack a => a -> SlackTextObject
mrkdwn = SlackMarkdownText . message

instance FromJSON SlackTextObject where
  parseJSON = withObject "SlackTextObject" $ \obj -> do
    (slackTextType :: Text) <- obj .: "type"
    case slackTextType of
      "text" -> do
        text <- obj .: "text"
        pure . SlackPlainText . SlackText $ lines text
      "mrkdwn" -> do
        text <- obj .: "text"
        pure . SlackMarkdownText . SlackText $ lines text
      _ -> fail "Unknown SlackTextObject type, must be one of ['text', 'mrkdwn']"

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
    object $
      [ "type" .= ("image" :: Text)
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
      _ -> fail "Unknown SlackContent type, must be one of ['mrkdwn', 'image']"

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

-- | Seemingly only documented at
--  <https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less>
--
--  They warn of undocumented element types. Joy.
data RichItem
  = RichItemText Text RichStyle
  | RichItemChannel ConversationId
  | RichItemUser UserId RichStyle
  | RichItemLink RichLinkAttrs
  | RichItemEmoji Text
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
      "user" -> do
        userId <- obj .: "user_id"
        style <- obj .:? "style" .!= mempty
        pure $ RichItemUser userId style
      _ -> pure $ RichItemOther kind (Object obj)

data RichTextSectionItem
  = RichTextSectionItemRichText [RichItem]
  | RichTextSectionItemUnknown Text Value
  deriving stock (Eq, Show)

instance FromJSON RichTextSectionItem where
  parseJSON = withObject "RichTextSectionItem" \obj -> do
    kind <- obj .: "type"
    case kind of
      "rich_text_section" -> do
        elts <- obj .: "elements"
        pure $ RichTextSectionItemRichText elts
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
  deriving stock (Eq)

instance ToJSON SlackAccessory where
  toJSON (SlackButtonAccessory btn) = toJSON btn

instance FromJSON SlackAccessory where
  parseJSON v = SlackButtonAccessory <$> parseJSON v

instance Show SlackAccessory where
  show (SlackButtonAccessory btn) = show btn

-- | Small helper function for constructing a section with a button accessory out of a button and text components
sectionWithButtonAccessory :: SlackAction -> SlackText -> SlackBlock
sectionWithButtonAccessory btn txt = SlackBlockSection txt (Just $ SlackButtonAccessory btn)

data SlackBlock
  = SlackBlockSection SlackText (Maybe SlackAccessory)
  | SlackBlockImage SlackImage
  | SlackBlockContext SlackContext
  | SlackBlockDivider
  | SlackBlockRichText RichText
  | SlackBlockActions (Maybe SlackBlockId) SlackActionList -- 1 to 5 elements
  | SlackBlockHeader SlackPlainTextOnly -- max length 150
  deriving stock (Eq)

instance Show SlackBlock where
  show (SlackBlockSection t Nothing) = show t
  show (SlackBlockSection t (Just mAccessory)) =
    show $ mconcat [show t, " [", show mAccessory, "]"]
  show (SlackBlockImage i) = show i
  show (SlackBlockContext contents) = show contents
  show SlackBlockDivider = "|"
  show (SlackBlockActions mBlockId as) =
    show $
      mconcat
        [ "actions("
        , show mBlockId
        , ") = ["
        , show $ intercalate ", " (map show (unrefine $ unSlackActionList as))
        , "]"
        ]
  show (SlackBlockRichText rt) = show rt
  show (SlackBlockHeader p) = show p

instance ToJSON SlackBlock where
  toJSON (SlackBlockSection slackText mSectionAccessory) =
    objectOptional
      [ "type" .=! ("section" :: Text)
      , "text" .=! SlackContentText slackText
      , "accessory" .=? mSectionAccessory
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

instance FromJSON SlackBlock where
  parseJSON = withObject "SlackBlock" $ \obj -> do
    (slackBlockType :: Text) <- obj .: "type"
    case slackBlockType of
      "section" -> do
        (sectionContentObj :: Value) <- obj .: "text"
        SlackContentText sectionContentText <- parseJSON sectionContentObj
        mSectionAccessory <- obj .:? "accessory"
        pure $ SlackBlockSection sectionContentText mSectionAccessory
      "context" -> do
        (contextElementsObj :: Value) <- obj .: "elements"
        slackContent <- parseJSON contextElementsObj
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
        pure . SlackBlockRichText $
          RichText
            { blockId = mBlockId
            , elements
            }
      "header" -> do
        (headerContentObj :: Value) <- obj .: "text"
        headerContentText <- parseJSON headerContentObj
        pure $ SlackBlockHeader headerContentText
      _ -> fail "Unknown SlackBlock type, must be one of ['section', 'context', 'image', 'divider', 'actions', 'rich_text', 'header']"

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
  markdown t = SlackMessage [SlackBlockSection t Nothing]

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
actions :: ToSlackActionList as => as -> SlackMessage
actions as = SlackMessage [SlackBlockActions Nothing $ toSlackActionList as]

-- | Generates interactive components such as buttons with a 'SlackBlockId'.
actionsWithBlockId :: ToSlackActionList as => SlackBlockId -> as -> SlackMessage
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
  SlackAction actionId $
    SlackButton
      { slackButtonText = buttonText
      , slackButtonUrl = unOptionalSetting buttonUrl
      , slackButtonValue = unOptionalSetting buttonValue
      , slackButtonStyle = unOptionalSetting buttonStyle
      , slackButtonConfirm = unOptionalSetting buttonConfirm
      }

-- | A divider block.
-- https://api.slack.com/reference/block-kit/blocks#divider
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

-- | All Slack Actions must have a 'SlackActionId' and one 'SlackActionComponent' (such as a button).
data SlackAction = SlackAction SlackActionId SlackActionComponent
  deriving stock (Eq)

instance Show SlackAction where
  show (SlackAction actionId component) = show actionId <> " " <> show component

-- | [Confirm dialog object](https://api.slack.com/reference/block-kit/composition-objects#confirm).
data SlackConfirmObject = SlackConfirmObject
  { slackConfirmTitle :: SlackPlainTextOnly -- max length 100
  , slackConfirmText :: SlackTextObject -- max length 300
  , slackConfirmConfirm :: SlackPlainTextOnly -- max length 30
  , slackConfirmDeny :: SlackPlainTextOnly -- max length 30
  , slackConfirmStyle :: Maybe SlackStyle
  }
  deriving stock (Eq)

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

-- | The component in a 'SlackAction'. Do not use directly.
-- Use the builder functions such as 'button' instead.
data SlackActionComponent = SlackButton
  { slackButtonText :: SlackButtonText -- max length 75, may truncate to ~30
  , slackButtonUrl :: Maybe (NonEmptyText 3000) -- max length 3000
  , slackButtonValue :: Maybe (NonEmptyText 2000) -- max length 2000
  , slackButtonStyle :: Maybe SlackStyle
  , slackButtonConfirm :: Maybe SlackConfirmObject
  }
  deriving stock (Eq)

instance FromJSON SlackActionComponent where
  parseJSON = withObject "SlactActionComponent" $ \obj -> do
    (slackActionType :: Text) <- obj .: "type"
    case slackActionType of
      "button" -> do
        text <- obj .: "text"
        slackButtonText <- text .: "text"
        slackButtonUrl <- obj .:? "url"
        slackButtonValue <- obj .:? "value"
        slackButtonStyle <- obj .:? "style"
        slackButtonConfirm <- obj .:? "confirm"
        pure $ SlackButton {..}
      _ -> fail "Unknown SlackActionComponent type, must be one of ['button']"

instance Show SlackActionComponent where
  show SlackButton {..} = "[button " <> show slackButtonText <> "]"

instance ToJSON SlackAction where
  toJSON (SlackAction actionId SlackButton {..}) =
    objectOptional
      [ "type" .=! ("button" :: Text)
      , "action_id" .=! actionId
      , "text" .=! plaintext slackButtonText
      , "url" .=? slackButtonUrl
      , "value" .=? slackButtonValue
      , "style" .=? slackButtonStyle
      , "confirm" .=? slackButtonConfirm
      ]

instance FromJSON SlackAction where
  parseJSON = withObject "SlackAction" $ \obj -> do
    actionId <- obj .: "action_id"
    slackActionComponent <- parseJSON $ Object obj
    pure $ SlackAction actionId slackActionComponent
