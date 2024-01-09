{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------

----------------------------------------------------------------------

-- |
-- Module: Web.Slack.Api
-- Description:
module Web.Slack.Api (
  TestReq (..),
  mkTestReq,
  TestRsp (..),
) where

-- FIXME: Web.Slack.Prelude

-- aeson

-- base

-- deepseq
import Control.DeepSeq (NFData)
import Data.Aeson.TH
-- http-api-data

-- slack-web

-- text
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.FormUrlEncoded
import Web.Slack.Util
import Prelude

data TestReq = TestReq
  { testReqError :: Maybe Text
  , testReqFoo :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)

instance NFData TestReq

$(deriveJSON (jsonOpts "testReq") ''TestReq)

instance ToForm TestReq where
  toForm =
    genericToForm (formOpts "testReq")

mkTestReq :: TestReq
mkTestReq =
  TestReq
    { testReqError = Nothing
    , testReqFoo = Nothing
    }

data TestRsp = TestRsp
  { testRspArgs :: Maybe TestReq
  }
  deriving stock (Eq, Generic, Show)

instance NFData TestRsp

$(deriveFromJSON (jsonOpts "testRsp") ''TestRsp)
