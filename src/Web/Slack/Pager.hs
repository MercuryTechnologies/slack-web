module Web.Slack.Pager (
  Response,
  LoadPage,
  loadingPage,
  fetchAllBy,
  module Web.Slack.Pager.Types,
) where

import Data.Kind (Type)
import Web.Slack.Common qualified as Common
import Web.Slack.Pager.Types
import Web.Slack.Prelude

type Response a = Either Common.SlackClientError a

-- | Represents an action which returns a paginated response from Slack.
--   Every time calling the action, it performs a request with a new cursor
--   to get the next page.
--   If there is no more response, the action returns an empty list.
type LoadPage m a = m (Response [a])

type LoadPage :: forall {k}. (Type -> k) -> Type -> k

-- | Utility function for 'LoadPage'. Perform the 'LoadPage' action to call
--   the function with the loaded page, until an empty page is loaded.
loadingPage :: (Monad m, Monoid n) => LoadPage m a -> (Response [a] -> m n) -> m n
loadingPage loadPage usePage = go mempty
  where
    go result = do
      epage <- loadPage
      case epage of
        Right page ->
          if null page
            then return result
            else (go $!) . (result <>) =<< usePage epage
        Left e -> (result <>) <$> usePage (Left e)

fetchAllBy ::
  ( MonadIO m
  , PagedRequest req
  , PagedResponse resp
  ) =>
  (req -> m (Response resp)) ->
  req ->
  m (LoadPage m (ResponseObject resp))
fetchAllBy sendRequest initialRequest = do
  cursorRef <- liftIO $ newIORef Nothing

  let requestFromCursor cursor = setCursor cursor initialRequest
      collectAndUpdateCursor resp = do
        let newCursor = responseMetadataNextCursor =<< getResponseMetadata resp
            -- emptyCursor is used for the marker to show that there are no more pages.
            cursorToSave = if isNothing newCursor then emptyCursor else newCursor
        writeIORef cursorRef cursorToSave
        return $ getResponseData resp

  return $ do
    cursor <- liftIO $ readIORef cursorRef
    if cursor == emptyCursor
      then return $ Right []
      else
        traverse (liftIO . collectAndUpdateCursor)
          =<< sendRequest (requestFromCursor cursor)
  where
    -- Used for the marker to show that there are no more pages.
    emptyCursor = Just $ Common.Cursor ""
