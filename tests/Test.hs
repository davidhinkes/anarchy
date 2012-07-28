import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Text.JSON
import Network.Anarchy
import Network.Anarchy.Server (State(..))
import Test.QuickCheck.Arbitrary
import qualified Data.ByteString.Char8 as B

main = defaultMain tests

instance (Arbitrary a) => Arbitrary (UniqueMessage a) where
  arbitrary = do
   x <- arbitrary
   y <- arbitrary
   z <- arbitrary
   return $ UniqueMessage x y z 

instance Arbitrary HostPort  where
  arbitrary = do
   x <- arbitrary
   y <- arbitrary
   return $ HostPort x y 

instance Arbitrary B.ByteString where
  arbitrary = do
    x <- arbitrary
    return . B.pack $ x

instance Arbitrary (State) where
  arbitrary = do
   x <- arbitrary
   y <- arbitrary
   z <- arbitrary
   return $ State x y z 

encodeDecodeProp  m = case (decode (encode m)) of
        Ok (m') -> m == m'
        _ -> False

tests = [
  testProperty "HostPort" (encodeDecodeProp :: UniqueMessage HostPort -> Bool),
  testProperty "HostPort" (encodeDecodeProp :: UniqueMessage State -> Bool)
  ]
