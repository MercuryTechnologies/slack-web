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

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Im =
  Im
    { imId :: Text
    , imIsIm :: Bool
    , imUser :: Text
    , imCreated :: POSIXTime
    , imIsUserDeleted :: Bool
    }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOpts "im") ''Im)

data ListRsp =
  ListRsp
    { listRspOk :: Bool
    , listRspIms :: [Im]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
