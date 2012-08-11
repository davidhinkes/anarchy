import Data.Set
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Text.JSON
import Network.Anarchy
import Network.Anarchy.Server (ServerState(..))
import Network.Anarchy.Server.Internal
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

instance Arbitrary (ServerState) where
  arbitrary = do
   x <- arbitrary
   y <- arbitrary
   z <- arbitrary
   return $ ServerState x y z 

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = do
    x <- arbitrary
    return $ fromList x

encodeDecodeProp  m = case (decode (encode m)) of
        Ok (m') -> m == m'
        _ -> False

distanceProp hp = distance hp hp == 0
distanceProp' a b = distance a b >= 0

tests = [
  testProperty "HostPort" (encodeDecodeProp :: UniqueMessage HostPort -> Bool),
  testProperty "ServerState" (encodeDecodeProp :: UniqueMessage ServerState -> Bool),
  testProperty "HostPortDistance" (distanceProp :: HostPort -> Bool),
  testProperty "HostPortDistance'" (distanceProp' :: HostPort -> HostPort -> Bool)
  ]
