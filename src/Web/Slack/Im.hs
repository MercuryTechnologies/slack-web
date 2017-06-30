{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Im
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Im
  ( Im(..)
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Util
import Web.Slack.Common

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Im =
  Im
    { imId :: Text
    , imIsIm :: Bool
    , imUser :: UserId
    , imCreated :: POSIXTime
    , imIsUserDeleted :: Bool
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "im") ''Im)

data ListRsp =
  ListRsp
    { listRspIms :: [Im]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
