{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Pager.Types where

import Web.Slack.Prelude
import Web.Slack.Util

newtype Cursor = Cursor {unCursor :: Text}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, ToHttpApiData)

newtype ResponseMetadata = ResponseMetadata {responseMetadataNextCursor :: Maybe Cursor}
  deriving stock (Eq, Show, Generic)

instance NFData ResponseMetadata

$(deriveJSON (jsonOpts "responseMetadata") ''ResponseMetadata)

class PagedRequest a where
  setCursor :: Maybe Cursor -> a -> a

class PagedResponse a where
  type ResponseObject a
  getResponseMetadata :: a -> Maybe ResponseMetadata
  getResponseData :: a -> [ResponseObject a]
