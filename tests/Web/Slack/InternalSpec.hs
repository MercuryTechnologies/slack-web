module Web.Slack.InternalSpec where

import JSONGolden
import TestImport
import Web.Slack.Internal

-- | Parses nothing and succeeds!
data NoJSONExpectations = NoJSONExpectations
  deriving stock (Show)

instance FromJSON NoJSONExpectations where
  parseJSON _ = pure NoJSONExpectations

spec :: Spec
spec = describe "Common infrastructure" do
  describe "Response parsing" do
    -- FIXME(jadel): discards warnings for successful responses! seems like we
    -- need to improve this API
    oneGoldenTestDecode @(ResponseJSON NoJSONExpectations) "metadata_example"
    oneGoldenTestDecode @(ResponseJSON NoJSONExpectations) "failed_view_publish"
