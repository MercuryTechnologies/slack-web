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

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)


-- |
--
--

data TestRsp =
  TestRsp
    { testRspOk :: Bool
    , testRspUrl :: Text
    , testRspTeam :: Text
    , testRspUser :: Text
    , testRspTeamId :: Text
    , testRspUserId :: Text
    , testRspEnterpriseId :: Maybe Text
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "testRsp") ''TestRsp)
