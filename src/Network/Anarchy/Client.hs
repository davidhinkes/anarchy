module Network.Anarchy.Client (
  ClientOp()
  ,Options(..)
  ,run
  ,register_
  ,hostCheck_
  ,addClients_
  --,register
  --,hostCheck
  ,addClients
) where

import qualified Control.Exception as E
import Control.Monad.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Network.HTTP
import Network.HTTP.Base
import Network.Stream
import Network.URI
import Text.JSON (encode)
import Network.Anarchy

-- Internal function.
resultToMaybeT :: Monad m => Result a -> MaybeT m a
resultToMaybeT (Right x) = return x
resultToMaybeT _ =  MaybeT (return Nothing)

-- Internal function.
handleException (E.SomeException e) = return . Left . ErrorMisc . show $ e

data Options = Options {
  hostport :: HostPort
}

data ClientOp a = ClientOp {
  runClientOp :: Options -> MaybeT IO a
}

instance Monad ClientOp where
  co >>= f = ClientOp f' where
    f' options = (runClientOp co options) >>= (\x -> runClientOp (f x) options)
  return a = ClientOp f where
    f _ = return a

run :: Options -> ClientOp a -> IO (Maybe a)
run os op = runMaybeT . (runClientOp op) $ os

getServerHostPort :: ClientOp HostPort
getServerHostPort = ClientOp f
  where f ops = return (hostport ops)

addClients_ :: [ HostPort ] -> ClientOp ()
addClients_ msg = ClientOp f where
  f os = do
    let HostPort h p = hostport os 
    let uriAuth = URIAuth "" h (":" ++ (show p))
    let uri = URI "http:" (Just uriAuth) "/addclients" "" ""
    let request = mkRequest PUT uri
    let request' = setRequestBody request ("text/json", encode msg)
    response <- lift . E.catch (simpleHTTP request') $ handleException
    return ()


hostCheck_ :: ClientOp String
hostCheck_ = ClientOp f where
  f os = do
    let hp = hostport os
    let HostPort h p = hp
    let uriAuth = URIAuth "" h (":" ++ (show p))
    let uri = URI "http:" (Just uriAuth) "/hostcheck" "" ""
    let request = Request uri GET [] ""
    result <- lift . E.catch (simpleHTTP request) $ handleException
    case result of
      Right x -> return . rspBody $ x
      Left _ -> MaybeT $ return Nothing

register_ :: UniqueMessage HostPort -> ClientOp ()
register_ msg = ClientOp f where
  f (Options tohp) = do
    let HostPort h p = tohp
    let uriAuth = URIAuth "" h (":" ++ (show p))
    let uri = URI "http:" (Just uriAuth) "/register" "" ""
    let request = setRequestBody (mkRequest PUT uri) ("text/json", encode msg)
    result <- lift . E.catch (simpleHTTP request) $ handleException
    case result of
      Right x -> return ()
      Left _ -> MaybeT $ return Nothing

register :: HostPort -> UniqueMessage HostPort -> IO ()
register tohp msg = do
  let HostPort h p = tohp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/register" "" ""
  let request = mkRequest PUT uri
  let request' = setRequestBody request ("text/json", encode msg)
  response <- simpleHTTP $ request'
  return ()

addClients :: HostPort -> [ HostPort ] -> IO ()
addClients tohp msg = do
  let HostPort h p = tohp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/addclients" "" ""
  let request = mkRequest PUT uri
  let request' = setRequestBody request ("text/json", encode msg)
  response <-simpleHTTP $ request'
  return ()

hostCheck :: HostPort -> MaybeT IO String
hostCheck hp = do
  let HostPort h p = hp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/hostcheck" "" ""
  let request = Request uri GET [] ""
  result <- lift . simpleHTTP $ request
  response <- resultToMaybeT result
  return $ rspBody response
