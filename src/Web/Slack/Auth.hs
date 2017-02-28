{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Auth
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)


-- |
--
--

newtype TestReq =
  TestReq
    { testReqToken :: Text
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "testReq") ''TestReq)


-- |
--
--

instance ToForm TestReq where
  toForm =
    genericToForm (formOpts "testReq")


-- |
--
--

mkTestReq
  :: Text
  -> TestReq
mkTestReq =
  TestReq


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
