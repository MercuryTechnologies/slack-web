{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Auth
-- Description:
--
--
--
----------------------------------------------------------------------


module Web.Slack.Auth
  where

-- FIXME: Web.Slack.Prelude
import Prelude

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData)

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)


-- |
--
--

data TestRsp =
  TestRsp
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
