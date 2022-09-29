{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.PagerSpec (spec) where

-- base
import Control.Exception (throwIO)
import Data.Maybe (fromJust)
-- fakepull

-- hspec

-- slack-web

-- text
import Data.Text qualified as Text
-- time
import Data.Time.Clock (addUTCTime, getCurrentTime, nominalDay)
import Test.Hspec
import Test.Pull.Fake.IO (FakeStream, newFakeStream, pull)
import Web.Slack.Common
  ( Cursor (..),
    Message (..),
    MessageType (..),
    SlackMessageText (..),
    mkSlackTimestamp,
  )
import Web.Slack.Conversation
  ( ConversationId (ConversationId),
    HistoryReq (..),
    HistoryRsp (..),
    RepliesReq (..),
    ResponseMetadata (..),
    mkHistoryReq,
    mkRepliesReq,
  )
import Web.Slack.Pager
import Web.Slack.Types (UserId (..))

stubbedSendRequest :: FakeStream (Response HistoryRsp) -> a -> IO (Response HistoryRsp)
stubbedSendRequest stream _request = fromJust <$> pull stream

spec :: Spec
spec = do
  let prepare = do
        nowUtc <- getCurrentTime
        let now = mkSlackTimestamp nowUtc
            oldest = mkSlackTimestamp $ addUTCTime (nominalDay * negate 20) nowUtc
            messagesPerPage = 3
            allResponses :: [Response HistoryRsp]
            allResponses = do
              -- According to https://api.slack.com/docs/pagination,
              -- The last page's cursor can be either an empty string, null, or non-exisitent in the object.
              (pageN, cursor) <- zip [1 .. 3] ["cursor1=", "cursor2=", ""]
              let pageNT = Text.pack (show pageN)
              pure . Right $
                HistoryRsp
                  { historyRspMessages = do
                      messageN <- [1 .. messagesPerPage]
                      let messagesPerPageNDT = fromIntegral messagesPerPage
                          messageNDT = fromIntegral messageN
                          messageNT = Text.pack (show messageN)
                          createdBefore = negate $ nominalDay * ((pageN - 1) * messagesPerPageNDT + messageNDT)
                      pure $
                        Message
                          MessageTypeMessage
                          (Just . UserId $ "U" <> pageNT <> messageNT)
                          (SlackMessageText $ "message " <> pageNT <> "-" <> messageNT)
                          (mkSlackTimestamp $ addUTCTime createdBefore nowUtc)
                  , historyRspResponseMetadata = Just . ResponseMetadata . Just $ Cursor cursor
                  }
        responsesToReturn <- newFakeStream allResponses
        return (now, oldest, messagesPerPage, allResponses, responsesToReturn)

  describe "conversationsHistoryAllBy" $
    it "collect all results by sending requests" $ do
      (now, oldest, messagesPerPage, allResponses, responsesToReturn) <- prepare
      let initialRequest =
            (mkHistoryReq (ConversationId "C01234567"))
              { historyReqCount = messagesPerPage
              , historyReqLatest = Just now
              , historyReqOldest = Just oldest
              , historyReqInclusive = False
              }
      loadPage <- conversationsHistoryAllBy (stubbedSendRequest responsesToReturn) initialRequest
      let actual = unfoldPageM $ either throwIO return =<< loadPage
      expected <- fmap (map historyRspMessages) . either throwIO return $ sequenceA allResponses
      actual `shouldReturn` expected

  describe "repliesFetchAllBy" $
    it "collect all results by sending requests" $ do
      (now, oldest, messagesPerPage, allResponses, responsesToReturn) <- prepare
      let initialRequest =
            (mkRepliesReq (ConversationId "C98765432") oldest)
              { repliesReqLimit = messagesPerPage
              , repliesReqLatest = Just now
              , repliesReqOldest = Just oldest
              , repliesReqInclusive = False
              }
      loadPage <- repliesFetchAllBy (stubbedSendRequest responsesToReturn) initialRequest
      let actual = unfoldPageM $ either throwIO return =<< loadPage
      expected <- fmap (map historyRspMessages) . either throwIO return $ sequenceA allResponses
      actual `shouldReturn` expected

-- | Runs the given action repeatedly until it returns an empty list.
unfoldPageM :: Monad m => m [a] -> m [[a]]
unfoldPageM act = reverse <$> go []
  where
    go accum = do
      x <- act
      case x of
        [] -> return accum
        xs -> go $! xs : accum
