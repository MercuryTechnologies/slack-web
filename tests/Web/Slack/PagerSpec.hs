{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.PagerSpec (spec) where

-- base
import Control.Exception (throwIO)
import Data.Maybe (fromJust)

-- fakepull
import Test.Pull.Fake.IO (FakeStream, pull, newFakeStream)

-- hspec
import Test.Hspec

-- slack-web
import Web.Slack.Pager
import Web.Slack.Common
  ( mkSlackTimestamp
  , Cursor (..)
  , Message (..)
  , MessageType (..)
  , SlackMessageText (..)
  )
import Web.Slack.Conversation
  ( ConversationId (ConversationId)
  , mkHistoryReq
  , mkRepliesReq
  , HistoryReq (..)
  , HistoryRsp (..)
  , RepliesReq (..)
  , ResponseMetadata (..)
  )
import Web.Slack.Types (UserId (..))

-- text
import qualified Data.Text as Text

-- time
import Data.Time.Clock (getCurrentTime, nominalDay, addUTCTime)


stubbedSendRequest :: FakeStream (Response HistoryRsp) -> a -> IO (Response HistoryRsp)
stubbedSendRequest stream _request = fromJust <$> pull stream


spec :: Spec
spec = do
  describe "conversationsHistoryAllBy" $
    it "collect all results by sending requests" $ do
      nowUtc <- getCurrentTime
      let now = mkSlackTimestamp nowUtc
          oldest = mkSlackTimestamp $ addUTCTime (nominalDay * negate 20) nowUtc
          messagesPerPage = 3
          allResponses :: [Response HistoryRsp]
          allResponses = do
            -- According to https://api.slack.com/docs/pagination,
            -- The last page's cursor can be either an empty string, null, or non-exisitent in the object.
            (pageN, cursor) <- zip [1..3] ["cursor1=", "cursor2=", ""]
            let pageNT = Text.pack (show pageN)
            pure . Right $ HistoryRsp
              { historyRspMessages = do
                  messageN <- [1..messagesPerPage]
                  let messagesPerPageNDT = fromIntegral messagesPerPage
                      messageNDT = fromIntegral messageN
                      messageNT = Text.pack (show messageN)
                      createdBefore = negate $ nominalDay * ((pageN - 1) * messagesPerPageNDT + messageNDT)
                  pure $ Message
                      MessageTypeMessage
                      (Just . UserId $ "U" <> pageNT <> messageNT)
                      (SlackMessageText $ "message " <> pageNT <> "-" <> messageNT)
                      (mkSlackTimestamp $ addUTCTime createdBefore nowUtc)
              , historyRspResponseMetadata = Just . ResponseMetadata . Just $ Cursor cursor
              }
      responsesToReturn <- newFakeStream allResponses
      let initialRequest = (mkHistoryReq (ConversationId "C01234567"))
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
      nowUtc <- getCurrentTime
      let now = mkSlackTimestamp nowUtc
          oldest = mkSlackTimestamp $ addUTCTime (nominalDay * negate 10) nowUtc
          firstMessage = Message MessageTypeMessage (Just $ UserId "U00") (SlackMessageText "message 0.0") oldest
          messagesPerPage = 3
          allResponses :: [Response HistoryRsp]
          allResponses = do
            -- According to https://api.slack.com/docs/pagination,
            -- The last page's cursor can be either an empty string, null, or non-exisitent in the object.
            (pageN, cursor) <- zip [1..3] ["cursor1=", "cursor2=", ""]
            let pageNT = Text.pack (show pageN)
            pure . Right $ HistoryRsp
              { historyRspMessages = (firstMessage :) $ do
                  messageN <- [1..messagesPerPage]
                  let messagesPerPageNDT = fromIntegral messagesPerPage
                      messageNDT = fromIntegral messageN
                      messageNT = Text.pack (show messageN)
                      createdBefore = negate $ nominalDay * ((pageN - 1) * messagesPerPageNDT + messageNDT)
                  pure $ Message
                      MessageTypeMessage
                      (Just . UserId $ "U" <> pageNT <> messageNT)
                      (SlackMessageText $ "message " <> pageNT <> "-" <> messageNT)
                      (mkSlackTimestamp $ addUTCTime createdBefore nowUtc)
              , historyRspResponseMetadata = Just . ResponseMetadata . Just $ Cursor cursor
              }
      responsesToReturn <- newFakeStream allResponses
      let initialRequest = (mkRepliesReq (ConversationId "C98765432") oldest)
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
