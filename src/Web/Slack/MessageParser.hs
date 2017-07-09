{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | See https://api.slack.com/docs/message-formatting
--
module Web.Slack.MessageParser
  ( messageToHtml
  )
  where

-- base
import Control.Monad
import Data.Maybe
import Data.Monoid

-- megaparsec
import Text.Megaparsec

-- mtl
import Data.Functor.Identity

-- text
import Data.Text (Text)
import qualified Data.Text as T

newtype SlackUrl = SlackUrl { unSlackUrl :: Text }
  deriving (Show, Eq)

data SlackMsgItem
  = SlackMsgItemPlainText Text
  | SlackMsgItemBoldSection [SlackMsgItem]
  | SlackMsgItemItalicsSection [SlackMsgItem]
  | SlackMsgItemLink Text SlackUrl
  | SlackMsgItemInlineCodeSection Text
  | SlackMsgItemCodeSection Text
  | SlackMsgItemQuoted [SlackMsgItem]
  deriving (Show, Eq)

type SlackParser a = ParsecT Dec T.Text Identity a

parseMessage :: Text -> [SlackMsgItem]
parseMessage input = fromMaybe [SlackMsgItemPlainText input] $
  parseMaybe (some parseMessageItem) input

parseMessageItem :: SlackParser SlackMsgItem
parseMessageItem
  = parseBoldSection
  <|> parseItalicsSection
  <|> parseCode
  <|> parseInlineCode
  <|> parseLink
  <|> parseBlockQuote
  <|> parsePlainText
  <|> parseWhitespace

parsePlainText :: SlackParser SlackMsgItem
parsePlainText = SlackMsgItemPlainText . T.pack <$>
    someTill (noneOf stopChars) (void (lookAhead $ oneOf stopChars)
                                   <|> lookAhead boldEndSymbol
                                   <|> lookAhead italicsEndSymbol
                                   <|> lookAhead eof)
    where stopChars = [' ', '\n']

-- slack accepts bold/italics modifiers
-- only at word boundary. for instance 'my_word'
-- doesn't trigger an italics section.
parseWhitespace :: SlackParser SlackMsgItem
parseWhitespace = SlackMsgItemPlainText . T.pack <$> some (oneOf [' ', '\n'])

boldEndSymbol :: SlackParser ()
boldEndSymbol = void $ char '*' >> lookAhead (void (oneOf [' ', '\n', '_']) <|> eof)

italicsEndSymbol :: SlackParser ()
italicsEndSymbol = void $ char '_' >> lookAhead (void (oneOf [' ', '\n', '*']) <|> eof)

parseBoldSection :: SlackParser SlackMsgItem
parseBoldSection = fmap SlackMsgItemBoldSection $
  char '*' *> someTill parseMessageItem boldEndSymbol

parseItalicsSection :: SlackParser SlackMsgItem
parseItalicsSection = fmap SlackMsgItemItalicsSection $
  char '_' *> someTill parseMessageItem italicsEndSymbol

parseLink :: SlackParser SlackMsgItem
parseLink = do
  void (char '<')
  url <- SlackUrl . T.pack <$> some (noneOf ['|', '>'])
  let linkWithoutDesc = char '>' >> pure (SlackMsgItemLink (unSlackUrl url) url)
  let linkWithDesc = char '|' >> SlackMsgItemLink <$> ((T.pack <$> some (noneOf ['>'])) <* char '>') <*> pure url
  linkWithDesc <|> linkWithoutDesc

parseCode :: SlackParser SlackMsgItem
parseCode = SlackMsgItemCodeSection . T.pack <$>
  (string "```" >> manyTill anyChar (string "```"))

parseInlineCode :: SlackParser SlackMsgItem
parseInlineCode = SlackMsgItemInlineCodeSection . T.pack <$>
  (char '`' *> some (noneOf ['`']) <* char '`')

parseBlockQuote :: SlackParser SlackMsgItem
parseBlockQuote = SlackMsgItemQuoted <$>
  (string "&gt;" *> optional (char ' ') *> some parseMessageItem)

messageToHtml :: Text -> Text
messageToHtml = messageToHtml' . parseMessage

messageToHtml' :: [SlackMsgItem] -> Text
messageToHtml' = foldr ((<>) . msgItemToHtml) ""

msgItemToHtml :: SlackMsgItem -> Text
msgItemToHtml = \case
  SlackMsgItemPlainText txt -> txt
  SlackMsgItemBoldSection cts -> "<b>" <> messageToHtml' cts <> "</b>"
  SlackMsgItemItalicsSection cts -> "<i>" <> messageToHtml' cts <> "</i>"
  SlackMsgItemLink txt url -> "<a href='" <> unSlackUrl url <> "'>" <> txt <> "</a>"
  SlackMsgItemInlineCodeSection code -> "<code>" <> code <> "</code>"
  SlackMsgItemCodeSection code -> "<pre>" <> code <> "</pre>"
  SlackMsgItemQuoted items -> "<blockquote>" <> messageToHtml' items <> "</blockquote>"
