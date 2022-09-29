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
  ( TestReq(..)
  , mkTestReq
  , TestRsp(..)
  )
  where

-- FIXME: Web.Slack.Prelude
import Prelude

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- deepseq
import Control.DeepSeq (NFData)

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
  deriving stock (Eq, Generic, Show)

instance NFData TestReq


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

mkTestReq :: TestReq
mkTestReq =
  TestReq
    { testReqError = Nothing
    , testReqFoo = Nothing
    }


-- |
--
--

data TestRsp =
  TestRsp
    { testRspArgs :: Maybe TestReq
    }
  deriving stock (Eq, Generic, Show)

instance NFData TestRsp

-- |
--
--
$(deriveFromJSON (jsonOpts "testRsp") ''TestRsp)
