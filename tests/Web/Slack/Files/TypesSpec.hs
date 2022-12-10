module Web.Slack.Files.TypesSpec (spec) where

import JSONGolden
import TestImport
import Web.Slack.Files.Types

spec :: Spec
spec = describe "Types for Slack files" do
  describe "FileObject" do
    describe "FromJSON" do
      mapM_
        (oneGoldenTestDecode @FileObject)
        [ "example"
        , "real"
        ]
