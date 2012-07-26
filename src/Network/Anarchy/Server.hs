module Network.Anarchy.Server (runServer) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Hashable
import Happstack.Lite
import Happstack.Server (Request(..), askRq)
import System.Timeout
import Text.JSON

import Network.Anarchy
import qualified Network.Anarchy.Client as Client

data State = State {
  clients :: [ B.ByteString ],
  hostport :: Maybe HostPort,
  digests :: [Int]
}

instance JSON State where
  showJSON s = let cs =  ("clients", showJSON (clients s))
                   hp = ("hostport", showJSON (hostport s))
                   d = ("digests", showJSON (digests s))
               in  makeObj [ cs, hp, d]
  readJSON x = do
                 jsObject <- extractJSObject x
                 cs <- valFromObj "clients" jsObject
                 hp <- valFromObj "hostport" jsObject
                 d <- valFromObj "digests" jsObject
                 return (State cs hp d)

instance ToMessage State where
  toMessage s = LB.pack . encode . showJSON $ s
  toContentType _ = B.pack "text/json"

myApp :: MVar State -> ServerPart Response
myApp x = msum [
  dir "status" . status $ x,
  dir "register" . register $ x,
  dir "hostcheck" . hostcheck $ x
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

hostcheck :: MVar State -> ServerPart Response
hostcheck _ = do
  method GET
  Request { rqPeer = (host,_) } <- askRq
  return . toResponse $ host

register :: MVar State -> ServerPart Response
register x = do
  method PUT -- only handle PUT requests
  --port <- path (\x -> return x)
  Request { rqPeer = (host,_) } <- askRq
  return . toResponse $ ""

status :: MVar State -> ServerPart Response
status x = do
  method GET -- only handle GET requests
  val <- liftIO . readMVar $ x
  return . toResponse $ val

runServer :: ServerConfig -> HostPort -> IO ()
runServer config hp = do
  let p = Happstack.Lite.port config
  x <- newMVar (State [] Nothing [])
  id <- async $ serve (return config) $ myApp x
  r <- runMaybeT $ Client.hostcheck "hink.es" 8080
  case r of
    Nothing -> return ()
    Just h -> modifyMVar_ x (\s -> return ( s { hostport = Just (HostPort h p) } ) )
  wait id
