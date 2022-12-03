{-# LANGUAGE TemplateHaskell #-}

module JSONGolden (oneGoldenTestDecode, oneGoldenTestEncode) where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty qualified as AP
import Data.ByteString.Lazy qualified as LBS
import Data.Text (stripEnd)
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp (..), Lit (..))
import Language.Haskell.TH.Syntax.Compat (makeRelativeToProject)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Golden
import TestImport
import Text.Pretty.Simple (pShowNoColor)
import Type.Reflection

-- this requires the filepath hacking like this so it can be run from arbitrary
-- working directories
filename :: Text -> Text -> FilePath
filename tycon name = $(LitE . StringL <$> makeRelativeToProject "tests/golden") </> unpack tycon </> unpack name

typeName :: forall a. Typeable a => Text
typeName = pack . tyConName . typeRepTyCon $ typeRep @a

goldenTestDecode :: forall a. (FromJSON a, Show a, Typeable a) => Text -> LByteString -> Golden Text
goldenTestDecode name rawInput = do
  let output = either error id $ eitherDecode @a rawInput
      theTypeName = typeName @a
   in Golden
        { output = toStrict . pShowNoColor $ output
        , encodePretty = unpack
        , writeToFile = T.writeFile
        , -- deal with vim related EOF
          readFromFile = \fname -> stripEnd <$> T.readFile fname
        , goldenFile = filename theTypeName name ++ ".golden"
        , actualFile = Just $ filename theTypeName name ++ ".actual"
        , failFirstTime = True
        }

goldenTestEncode :: forall a. (ToJSON a, Typeable a) => Text -> a -> Golden Text
goldenTestEncode name value = do
  let output = AP.encodePretty @a value
      theTypeName = typeName @a
   in Golden
        { output = cs output
        , encodePretty = unpack
        , writeToFile = T.writeFile
        , -- deal with vim related EOF
          readFromFile = \fname -> stripEnd <$> T.readFile fname
        , goldenFile = filename theTypeName name ++ ".golden.json"
        , actualFile = Just $ filename theTypeName name ++ ".actual.json"
        , failFirstTime = True
        }

oneGoldenTestEncode :: forall a. (ToJSON a, Typeable a) => Text -> a -> SpecM () ()
oneGoldenTestEncode name value = do
  it (unpack name) $ goldenTestEncode @a name value

oneGoldenTestDecode :: forall a. (FromJSON a, Show a, Typeable a) => Text -> SpecM () ()
oneGoldenTestDecode name = do
  input <- runIO . LBS.readFile $ filename (typeName @a) name <> ".json"
  it (unpack name) $ goldenTestDecode @a name input
