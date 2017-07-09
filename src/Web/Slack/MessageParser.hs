{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | See https://api.slack.com/docs/message-formatting
--
module Web.Slack.MessageParser
  ( parseMessage
  , SlackMsgItem(..)
  , SlackUrl(..)
  , messageToHtml
  )
  where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid
import Data.Functor.Identity
import Text.Megaparsec
import Control.Monad.Trans.State

newtype SlackUrl = SlackUrl { unSlackUrl :: Text }
  deriving (Show, Eq)

data SlackMsgItem
  = SlackMsgItemPlainText
     { slackPlainTextBold :: Bool
     , slackPlainTextItalics :: Bool
     , slackPlainTextText :: Text
     }
  | SlackMsgItemLink Text SlackUrl
  | SlackMsgItemInlineCodeSection Text
  | SlackMsgItemCodeSection Text
  | SlackMsgItemQuoted [SlackMsgItem]
  | SlackMsgItemSymbol Text
  deriving (Show, Eq)

data SlackParserState = SlackParserState
  { slackParserStateBoldText :: Bool
  , slackParserStateItalicsText :: Bool
  }

initialSlackParserState :: SlackParserState
initialSlackParserState = SlackParserState False False

type SlackParser a = StateT SlackParserState (ParsecT Dec T.Text Identity) a

parseMessage :: Text -> [SlackMsgItem]
parseMessage input = fromMaybe [SlackMsgItemPlainText False False input] $
  fst <$> parseMaybe (runStateT (many parseMessageItem) initialSlackParserState) input

parseMessageItem :: SlackParser SlackMsgItem
parseMessageItem
  = parsePlainText
  <|> parseBoldSymbol
  <|> parseItalicsSymbol
  <|> parseCode
  <|> parseInlineCode
  <|> parseLink

parsePlainText :: SlackParser SlackMsgItem
parsePlainText = do
  st <- get
  SlackMsgItemPlainText
    (slackParserStateBoldText st) (slackParserStateItalicsText st) . T.pack <$>
    some (noneOf ['<', '`', '*', '_'])

parseBoldSymbol :: SlackParser SlackMsgItem
parseBoldSymbol = do
  void (char '*')
  modify (\st -> st {slackParserStateBoldText = not (slackParserStateBoldText st)})
  return (SlackMsgItemSymbol "*")

parseItalicsSymbol :: SlackParser SlackMsgItem
parseItalicsSymbol = do
  void (char '_')
  modify (\st -> st {slackParserStateItalicsText = not (slackParserStateItalicsText st)})
  return (SlackMsgItemSymbol "_")

parseLink :: SlackParser SlackMsgItem
parseLink = do
  void (char '<')
  url <- SlackUrl . T.pack <$> some (noneOf ['|', '>'])
  let linkWithoutDesc = char '>' >> pure (SlackMsgItemLink (unSlackUrl url) url)
  let linkWithDesc = char '|' >> SlackMsgItemLink <$> ((T.pack <$> some (noneOf ['>'])) <* char '>') <*> pure url
  linkWithDesc <|> linkWithoutDesc

parseCode :: SlackParser SlackMsgItem
parseCode = SlackMsgItemInlineCodeSection . T.pack <$>
  (string "```" >> manyTill anyChar (string "```"))

parseInlineCode :: SlackParser SlackMsgItem
parseInlineCode = SlackMsgItemInlineCodeSection . T.pack <$>
  (char '`' *> some (noneOf ['`']) <* char '`')

messageToHtml :: Text -> Text
messageToHtml = messageToHtml' . parseMessage

messageToHtml' :: [SlackMsgItem] -> Text
messageToHtml' = foldr ((<>) . msgItemToHtml) ""

msgItemToHtml :: SlackMsgItem -> Text
msgItemToHtml = \case
  SlackMsgItemPlainText True True txt -> "<b><i>" <> txt <> "</i></b>"
  SlackMsgItemPlainText True False txt -> "<b>" <> txt <> "</b>"
  SlackMsgItemPlainText False True txt -> "<i>" <> txt <> "</i>"
  SlackMsgItemPlainText False False txt -> txt
  SlackMsgItemLink txt url -> "<a href='" <> unSlackUrl url <> "'>" <> txt <> "</a>"
  SlackMsgItemInlineCodeSection code -> "<pre>" <> code <> "</pre>"
  SlackMsgItemCodeSection code -> "<pre>" <> code <> "</pre>"
  SlackMsgItemQuoted items -> "<blockquote>" <> messageToHtml' items <> "</blockquote>"
  SlackMsgItemSymbol _ -> ""
