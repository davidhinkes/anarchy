import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Hashable
import Data.Time.Clock.POSIX
import Happstack.Lite
import Happstack.Server (Request(..), askRq)
import System.Timeout
import Text.JSON

import Network.Anarchy.Client

type HostPort = String

data HashableMessage a = HashableMessage a String HostPort

instance (JSON a) => JSON (HashableMessage a) where
  showJSON (HashableMessage m t hp) = let t' = ("timestamp", showJSON t)
                                          hp' = ("hostport", showJSON hp)
                                          m' = ("contents", showJSON m)
                                      in makeObj [ t', hp', m' ]
  readJSON v = do jsObject <- extractJSObject v
                  t <- valFromObj "timestamp" jsObject
                  hp <- valFromObj "hostport" jsObject
                  m <- valFromObj "contents" jsObject
                  return $ HashableMessage m t hp

instance (Hashable a) => Hashable (HashableMessage a) where
  hash (HashableMessage a b c) = hash (a, b, c)

instance (ToMessage a, JSON a) => ToMessage (HashableMessage a) where
  toMessage s = LB.pack . encode . showJSON $ s
  toContentType _ = B.pack "text/json"

data State = State {
  clients :: [ B.ByteString ],
  port :: Int,
  host :: Maybe String,
  digests :: [Int]
}

mkMessage :: a -> String -> IO (HashableMessage a)
mkMessage m hostport = do
  t <- getPOSIXTime
  return $ HashableMessage m (show t) hostport

extractJSObject :: JSValue -> Result (JSObject JSValue)
extractJSObject (JSObject v) = return v
extractJSObject _ = Error "No JSObject"

instance JSON State where
  showJSON s = let cs =  ("clients", showJSON (clients s))
                   p = ("port", showJSON (Main.port s))
                   h = ("host", showJSON (Main.host s))
                   d = ("digests", showJSON (digests s))
               in  makeObj [ cs, p, h, d]
  readJSON x = do
                 jsObject <- extractJSObject x
                 cs <- valFromObj "clients" jsObject
                 p <- valFromObj "port" jsObject
                 h <- valFromObj "host" jsObject
                 d <- valFromObj "digests" jsObject
                 return (State cs p h d)

instance ToMessage State where
  toMessage s = LB.pack . encode . showJSON $ s
  toContentType _ = B.pack "text/json"

myApp :: MVar State -> ServerPart Response
myApp x = msum [
  dir "status" . status $ x,
  dir "up" . up $ x
--dir "networkupdate" . networkupdate $ x
  ]

{--
networkupdate :: MVar State -> ServerPart Response
networkupdate x = do
  method PUT -- only handle PUT requests
  id <- path (\x -> return x)
  liftIO $ modifyMVar_ x (\s -> return (s { digests = (id : (digests s)) }))
  return . toResponse $ ()
--}

up :: MVar State -> ServerPart Response
up x = do
  method PUT -- only handle PUT requests
  --port <- path (\x -> return x)
  Request { rqPeer = (host,_) } <- askRq
  return . toResponse $ host

status :: MVar State -> ServerPart Response
status x = do
  method GET -- only handle GET requests
  val <- liftIO . readMVar $ x
  return . toResponse $ val

main = do
  putStrLn "Anarchy."
  let serverConfig = defaultServerConfig { Happstack.Lite.port = 8080 }
  let p = Happstack.Lite.port serverConfig
  x <- newMVar (State [] p Nothing [])
  id <- async $ serve (return serverConfig) $ myApp x
  r <- runMaybeT $ Network.Anarchy.Client.up_ "hink.es" 8080
  case r of
    Nothing -> return ()
    Just h -> modifyMVar_ x (\s -> return ( s { host = Just h } ) )
  wait id
