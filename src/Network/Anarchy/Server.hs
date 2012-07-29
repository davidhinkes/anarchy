module Network.Anarchy.Server (ServerState(..), runServer, block) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Maybe
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Hashable
import Data.Set
import Data.List (sortBy)
import Happstack.Lite
import Happstack.Server (Request(..), askRq)
import Happstack.Server.Types
import System.Timeout
import Text.JSON

import Network.Anarchy
import qualified Network.Anarchy.Client as Client

data ServerState = ServerState {
  clients :: [ HostPort ],
  hostport :: Maybe HostPort,
  digests :: Set Int
} deriving (Eq, Show)

type Handle = (MVar ServerState, Async ())

instance JSON ServerState where
  showJSON s = let cs =  ("clients", showJSON (clients s))
                   hp = ("hostport", showJSON (hostport s))
                   d = ("digests", showJSON (digests s))
               in  makeObj [ cs, hp, d]
  readJSON x = do
                 jsObject <- extractJSObject x
                 cs <- valFromObj "clients" jsObject
                 hp <- valFromObj "hostport" jsObject
                 d <- valFromObj "digests" jsObject
                 return (ServerState cs hp d)

instance ToMessage ServerState where
  toMessage s = LB.pack . encode . showJSON $ s
  toContentType _ = B.pack "text/json"

myApp :: MVar ServerState -> ServerPart Response
myApp x = msum [
  dir "status" . status $ x,
  dir "register" . register $ x,
  dir "hostcheck" . hostcheck $ x
  ]

hostcheck :: MVar ServerState -> ServerPart Response
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

register :: MVar ServerState -> ServerPart Response
register x = do
  method PUT -- only handle PUT requests
  body <- getBody
  let msg = decode body :: Result (UniqueMessage HostPort)
  case (msg) of
    Ok (umhp) -> do
      s <- liftIO $ readMVar x
      liftIO $ modifyMVar_ x (registerHostPort umhp)
      let cs = clients s
      liftIO . sequence . Prelude.map (\hp -> forkIO $ Client.register hp umhp) $ cs
      return . toResponse $ "Ok"
    Error str  -> do
      return . toResponse $ "" 

hasMessage :: Hashable a => UniqueMessage a -> ServerState -> Bool
hasMessage um s =
  let h = hash um
  in member h $ digests s

insertMessageDigest :: Hashable a => UniqueMessage a -> State ServerState ()
insertMessageDigest um = do 
  s <- get
  put $ s {digests = insert (hash um) (digests s) }

distance :: HostPort -> HostPort -> Int
distance myHp hp = let
  myHpHash = p . hash $ myHp
  hpHash = p . hash $ hp
  in case (hpHash > myHpHash) of
    True -> hpHash - myHpHash
    False -> (maxBound - myHpHash) + (hpHash - minBound)
  where p x = if x < 0 then (-x) else x

filterHostPort :: HostPort -> [ HostPort ] -> [ HostPort ]
filterHostPort myHp hps =
  let hps' = sortBy (\ a b -> compare (distance myHp a) (distance myHp b)) hps
  in take 3 hps'

registerHostPort :: UniqueMessage HostPort -> ServerState -> IO ServerState
registerHostPort umhp s = case (hasMessage umhp s) of
  True -> return s
  False -> return $ execState sm s where
    sm = do
      insertMessageDigest umhp
      let UniqueMessage hp _ _ = umhp
      s <- get
      let Just myHp = hostport s
      put $ s { clients = filterHostPort myHp (hp : (clients s)) }

status :: MVar ServerState -> ServerPart Response
status x = do
  method GET -- only handle GET requests
  val <- liftIO . readMVar $ x
  return . toResponse $ val

updateHostPort :: HostPort -> Int -> MVar ServerState ->  MaybeT IO ()
updateHostPort hp p s = do
  h <- Client.hostcheck $ hp
  liftIO $ modifyMVar_ s (\x -> return ( x { hostport = Just (HostPort h p) } ) )

runServer :: ServerConfig -> HostPort -> IO Handle
runServer config hp = do
  let p = Happstack.Lite.port config
  x <- newMVar (ServerState [] Nothing empty)
  id <- async $ serve (return config) $ myApp x
  runMaybeT $ updateHostPort hp p x
  s <- readMVar x
  let Just myhp = hostport s
  msg <- mkMessage myhp myhp
  Client.register hp msg
  return (x, id)

block :: Handle -> IO ()
block (_, id) = wait id
