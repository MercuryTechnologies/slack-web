{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.Internal
  ( Response
  , conversationsHistoryAllBy
  , LoadPage
  ) where

-- base
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)

-- slack-web
import qualified Web.Slack.Conversation as Conversation
import qualified Web.Slack.Common as Common


-- | Public only for testing.
conversationsHistoryAllBy
  :: MonadIO m
  => (Conversation.HistoryReq -> m (Response Conversation.HistoryRsp))
  -- ^ Response generator
  -> Conversation.HistoryReq
  -- ^ The first request to send. _NOTE_: 'Conversation.historyReqCursor' is silently ignored.
  -> m (LoadPage m [Common.Message])
  -- ^ An action which returns a new page of messages every time called. 
  --   If there are no pages anymore, it returns an empty list.
conversationsHistoryAllBy sendRequest initialRequest = do
  cursorRef <- liftIO $ newIORef Nothing

  let collectAndUpdateCursor
        Conversation.HistoryRsp
          { Conversation.historyRspMessages
          , Conversation.historyRspResponseMetadata
          } = do
            let newCursor = join (fmap Conversation.responseMetadataNextCursor historyRspResponseMetadata)
                -- emptyCursor is used for the marker to show that there are no more pages.
                cursorToSave = if newCursor == Nothing then emptyCursor else newCursor
            writeIORef cursorRef cursorToSave
            return historyRspMessages

  return $ do
    cursor <- liftIO $ readIORef cursorRef
    if cursor == emptyCursor
      then
        return $ Right []
      else
        traverse (liftIO . collectAndUpdateCursor)
          =<< sendRequest initialRequest { Conversation.historyReqCursor = cursor }
 where
  -- Used for the marker to show that there are no more pages.
  emptyCursor = Just $ Common.Cursor ""


type Response a = Either Common.SlackClientError a


-- | Represents an action which returns a paginated response from Slack.
--   Every time calling the action, it performs a request with a new cursor
--   to get the next page.
--   If there is no more response, the action returns an empty list.
type LoadPage m a = m (Response a)
