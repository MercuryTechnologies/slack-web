{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Api
-- Description:
--
--
--
----------------------------------------------------------------------


module Web.Slack.Api
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

data TestReq =
  TestReq
    { testReqError :: Maybe Text
    , testReqFoo :: Maybe Text
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

data TestRsp =
  TestRsp
    { testRspOk :: Bool
    , testRspArgs :: Maybe TestReq
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "testRsp") ''TestRsp)
