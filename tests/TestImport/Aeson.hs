module TestImport.Aeson (aesonRoundtrips) where

import Data.Aeson.Types qualified as A
import TestImport

aesonRoundtrips :: forall a. (FromJSON a, ToJSON a, Eq a) => a -> Bool
aesonRoundtrips a =
  let encoded = toJSON a
      parsed = A.parse (parseJSON @a) encoded
      roundTwo = fmap (toJSON @a) parsed
   in -- The encoding is the same
      A.Success encoded == roundTwo
        -- AND the object itself is the same
        && A.Success a == parsed
