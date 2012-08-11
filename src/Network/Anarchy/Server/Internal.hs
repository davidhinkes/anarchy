module Network.Anarchy.Server.Internal where

import Data.Hashable
import Network.Anarchy
import Network.Anarchy

distance :: HostPort -> HostPort -> Int
distance myHp hp = let
  myHpHash = p . hash $ myHp
  hpHash = p . hash $ hp
  in case (hpHash >= myHpHash) of
    True -> hpHash - myHpHash
    False -> (maxBound - myHpHash) + (hpHash)
  where p x = if x < 0 then (-x) else x


