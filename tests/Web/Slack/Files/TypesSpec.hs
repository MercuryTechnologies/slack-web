{-# OPTIONS_GHC -Wno-orphans #-}
-- GHC told me to set it, and it compiles now 🤷
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Web.Slack.Files.TypesSpec (spec) where

import JSONGolden
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))
import TestImport
import TestImport.Aeson
import Web.Slack.Files.Types

deriving via GenericArbitrary FileMode instance Arbitrary FileMode

spec :: Spec
spec = describe "Types for Slack files" do
  describe "FileObject" do
    prop "FileMode roundtrips" $ aesonRoundtrips @FileMode
    describe "FromJSON" do
      mapM_
        (oneGoldenTestDecode @FileObject)
        [ "example"
        , "real"
        ]
