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
import Data.Aeson

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Util

-- text
import Data.Text (Text)


-- |
--
--

data TestRsp
  = TestRspError Text
  | TestRsp
    { testRspUrl :: Text
    , testRspTeam :: Text
    , testRspUser :: Text
    , testRspTeamId :: Text
    , testRspUserId :: Text
    , testRspEnterpriseId :: Maybe Text
    }
  deriving (Eq, Generic, Show)


instance FromJSON TestRsp where
  parseJSON = fromJsonWithOk "TestRsp" TestRspError $ \o ->
                TestRsp <$> o .: "url" <*> o .: "team" <*> o .: "user"
                        <*> o .: "team_id" <*> o .: "user_id"
                        <*> o .: "enterprise_id"
