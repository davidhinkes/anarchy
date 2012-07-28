module Network.Anarchy.Server (State(..), runServer, block) where

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
import Happstack.Server.Types
import System.Timeout
import Text.JSON

import Network.Anarchy
import qualified Network.Anarchy.Client as Client

data State = State {
  clients :: [ B.ByteString ],
  hostport :: Maybe HostPort,
  digests :: [Int]
} deriving (Eq, Show)

type Handle = (MVar State, Async ())

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
  ]

hostcheck :: MVar State -> ServerPart Response
hostcheck _ = do
  method GET
  Request { rqPeer = (host,_) } <- askRq
  return . toResponse $ host

getBody :: ServerPart String
getBody = do
  req <- askRq
  body <- liftIO $ takeRequestBody req
  case body of
    Just r -> return . LB.unpack . unBody $ r
    Nothing -> return "Nothing"

register :: MVar State -> ServerPart Response
register x = do
  method PUT -- only handle PUT requests
  body <- getBody
  let msg = decode body :: Result (UniqueMessage HostPort)
  case (msg) of
    Ok (UniqueMessage a _ _) -> return . toResponse $ "Ok"
    _ -> return . toResponse $ body

status :: MVar State -> ServerPart Response
status x = do
  method GET -- only handle GET requests
  val <- liftIO . readMVar $ x
  return . toResponse $ val

updateHostPort :: HostPort -> Int -> MVar State ->  MaybeT IO ()
updateHostPort hp p s = do
  h <- Client.hostcheck $ hp
  liftIO $ modifyMVar_ s (\x -> return ( x { hostport = Just (HostPort h p) } ) )

runServer :: ServerConfig -> HostPort -> IO Handle
runServer config hp = do
  let p = Happstack.Lite.port config
  x <- newMVar (State [] Nothing [])
  id <- async $ serve (return config) $ myApp x
  runMaybeT $ updateHostPort hp p x
  return (x, id)

block :: Handle -> IO ()
block (_, id) = wait id
