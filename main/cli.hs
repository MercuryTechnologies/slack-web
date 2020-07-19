{-# LANGUAGE LambdaCase #-}


-- base
import System.Environment (getEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

-- butcher
import UI.Butcher.Monadic

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

-- mtl
import Control.Monad.Reader (runReaderT)

-- pretty-simple
import Text.Pretty.Simple (pPrint, pShow)

-- slack-web
import qualified Web.Slack as Slack
import qualified Web.Slack.Common as Slack
import qualified Web.Slack.Conversation as SlackConversation

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy

-- time
import Data.Time.Clock (getCurrentTime, nominalDay, addUTCTime)

-- servant-client-core
import Servant.Client.Core (ClientError(..), Response, ResponseF(..))


main :: IO ()
main = do
  apiConfig <- Slack.mkSlackConfig . Text.pack =<< getEnv "SLACK_API_TOKEN"
  mainFromCmdParserWithHelpDesc $ \helpDesc -> do
    addHelpCommand helpDesc
    addCmd "conversations.list" . addCmdImpl $ do
      let listReq = SlackConversation.ListReq
            { SlackConversation.listReqExcludeArchived = Just True
            , SlackConversation.listReqTypes =
              [ SlackConversation.PublicChannelType
              , SlackConversation.PrivateChannelType
              , SlackConversation.MpimType
              , SlackConversation.ImType
              ]
            }
      Slack.conversationsList listReq
        `runReaderT` apiConfig >>= \case
          Right (SlackConversation.ListRsp cs) -> do
            pPrint cs
          Left err -> do
            peepInResponseBody err
            hPutStrLn stderr "Error when fetching the list of conversations:"
            die . TextLazy.unpack $ pShow err

    addCmd "conversations.history" $ do
      conversationId <- addParamString "CONVERSATION_ID" (paramHelpStr "ID of the conversation to fetch")
      addCmdImpl $ do
        nowUtc <- getCurrentTime
        let now = Slack.mkSlackTimestamp nowUtc
            thirtyDaysAgo = Slack.mkSlackTimestamp $ addUTCTime (nominalDay * negate 30) nowUtc
            histReq = Slack.HistoryReq
              { Slack.historyReqChannel = Text.pack conversationId
              , Slack.historyReqCount = 5
              -- NOTE: It seems that slack returns error when either `latest` or `oldest` is omitted,
              --       while the document says both of them are optional!
              , Slack.historyReqLatest = Just now
              , Slack.historyReqOldest = Just thirtyDaysAgo 
              , Slack.historyReqInclusive = True
              }
        Slack.conversationsHistory histReq
          `runReaderT` apiConfig >>= \case
            Right rsp -> do
              pPrint rsp
            Left err -> do
              peepInResponseBody err
              hPutStrLn stderr "Error when fetching the history of conversations:"
              die . TextLazy.unpack $ pShow err


peepInResponseBody err = do
  {- Uncomment these lines when you want to see the JSON in the reponse body.
  case err of
    Slack.ServantError (DecodeFailure _ res) ->
      BL.putStrLn $ responseBody res
    _ -> return ()
  -}
  return ()
