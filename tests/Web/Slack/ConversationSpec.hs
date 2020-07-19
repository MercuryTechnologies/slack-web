{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Slack.ConversationSpec
  ( spec
  ) where

-- aeson
import Data.Aeson

-- base
import Data.Maybe

-- hspec
import Test.Hspec
import Test.Hspec.QuickCheck

-- QuickCheck
import Test.QuickCheck

-- quickcheck-instances
import Test.QuickCheck.Instances ()

-- slack-web
import Web.Slack.Common
import Web.Slack.Conversation

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX


instance Arbitrary MessageType where
  arbitrary = return MessageTypeMessage

instance Arbitrary UserId where
  arbitrary = fromJust . decode . encode <$> (arbitrary :: Gen Text)

instance Arbitrary SlackTimestamp where
  -- NOTE: `arbitrary :: Gen UTCTime` only for positive POSIX Second is really slow!
  arbitrary = mkSlackTimestamp . posixSecondsToUTCTime . fromIntegral <$> (arbitrary :: Gen Word)

instance Arbitrary Message where
  arbitrary =
    Message <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Topic where
  arbitrary = Topic <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Purpose where
  arbitrary = Purpose <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ChannelConversation where
  arbitrary = ChannelConversation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary GroupConversation where
  arbitrary = GroupConversation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ImConversation where
  arbitrary = ImConversation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Conversation where
  arbitrary = oneof [Channel <$> arbitrary, Group <$> arbitrary, Im <$> arbitrary]


deriving instance Arbitrary ConversationId
deriving instance Arbitrary SlackMessageText
deriving instance Arbitrary TeamId


spec :: Spec
spec = describe "ToJSON and FromJSON for Conversation" $ do
    prop "the encoded json is decoded as " $ \conversation -> do
      actual <- either fail return . eitherDecode $ encode conversation
      actual `shouldBe` (conversation :: Conversation)
