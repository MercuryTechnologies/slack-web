{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Slack.ConversationSpec (
  spec,
) where

-- aeson
import Data.Aeson
-- time
import Data.Time.Clock.POSIX
import JSONGolden
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TestImport
import Web.Slack.Common
import Web.Slack.Conversation

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
  arbitrary =
    ChannelConversation
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
  arbitrary =
    GroupConversation
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
  arbitrary =
    ImConversation
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Conversation where
  arbitrary = oneof [Channel <$> arbitrary, Group <$> arbitrary, Im <$> arbitrary]

deriving newtype instance Arbitrary ConversationId

deriving newtype instance Arbitrary SlackMessageText

deriving newtype instance Arbitrary TeamId

spec :: Spec
spec = describe "ToJSON and FromJSON for Conversation" $ do
  prop "the encoded json is decoded as " $ \conversation -> do
    actual <- either fail return . eitherDecode $ encode conversation
    actual `shouldBe` (conversation :: Conversation)

  describe "Golden tests" $ do
    mapM_ (oneGoldenTestDecode @Conversation) ["shared_channel"]
    mapM_ (oneGoldenTestDecode @InfoRsp) ["general"]
    mapM_ (oneGoldenTestDecode @JoinRsp) ["test"]
    mapM_ (oneGoldenTestDecode @MembersRsp) ["test"]

  it "errors accurately if no variant matches" $ do
    let badData =
          object
            [ ("is_group", Bool False)
            , ("is_im", Bool False)
            , ("is_channel", Bool False)
            ]
    fromJSON @Conversation badData
      `shouldBe` Error "parsing a Conversation failed: neither channel, group, nor im: expected Conversation, but encountered Object"

  it "has good errors if a variant matches but is missing fields" $ do
    let badData =
          object
            [ ("is_group", Bool False)
            , ("is_im", Bool False)
            , ("is_channel", Bool True)
            ]
    fromJSON @Conversation badData
      `shouldBe` Error "When parsing the record ChannelConversation of type Web.Slack.Conversation.ChannelConversation the key id was not present."
