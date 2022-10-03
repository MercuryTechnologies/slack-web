{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Auth
-- Description:
module Web.Slack.Auth where

-- FIXME: Web.Slack.Prelude

-- aeson

-- base

-- deepseq
import Control.DeepSeq (NFData)
import Data.Aeson.TH
-- slack-web

-- text
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Slack.Util
import Prelude

data TestRsp = TestRsp
  { testRspUrl :: Text
  , testRspTeam :: Text
  , testRspUser :: Text
  , testRspTeamId :: Text
  , testRspUserId :: Text
  , testRspEnterpriseId :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

instance NFData TestRsp

$(deriveJSON (jsonOpts "testRsp") ''TestRsp)
