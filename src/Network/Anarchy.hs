module Network.Anarchy where

import Text.JSON
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Hashable
import Data.Time.Clock.POSIX
import Happstack.Lite

data HostPort = HostPort String Int deriving (Eq, Show)

instance Hashable HostPort where
  hash (HostPort h p) = hash (h, p)

instance JSON HostPort where
  showJSON (HostPort h p) = let h' = ("host", showJSON h)
                                p' = ("port", showJSON p)
                            in makeObj [ h', p' ]
  readJSON v = do jsObject <- extractJSObject v
                  h <- valFromObj "host" jsObject
                  p <- valFromObj "port" jsObject
                  return $ HostPort h p

data UniqueMessage a = UniqueMessage a String HostPort deriving (Eq, Show)

instance (JSON a) => JSON (UniqueMessage a) where
  showJSON (UniqueMessage m t hp) = let t' = ("timestamp", showJSON t)
                                        hp' = ("hostport", showJSON hp)
                                        m' = ("contents", showJSON m)
                                    in makeObj [ t', hp', m' ]
  readJSON v = do jsObject <- extractJSObject v
                  t <- valFromObj "timestamp" jsObject
                  hp <- valFromObj "hostport" jsObject
                  m <- valFromObj "contents" jsObject
                  return $ UniqueMessage m t hp

instance (Hashable a) => Hashable (UniqueMessage a) where
  hash (UniqueMessage a b c) = hash (a, b, c)

instance (ToMessage a, JSON a) => ToMessage (UniqueMessage a) where
  toMessage s = LB.pack . encode . showJSON $ s
  toContentType _ = B.pack "text/json"

mkMessage :: a -> HostPort -> IO (UniqueMessage a)
mkMessage m hostport = do
  t <- getPOSIXTime
  return $ UniqueMessage m (show t) hostport

extractJSObject :: JSValue -> Result (JSObject JSValue)
extractJSObject (JSObject v) = return v
extractJSObject _ = Error "No JSObject"
