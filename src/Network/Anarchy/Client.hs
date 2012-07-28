module Network.Anarchy.Client (
  register
  ,hostcheck
) where

import Control.Monad.Maybe
import Control.Monad.Trans.Class
import Network.HTTP
import Network.HTTP.Base
import Network.Stream
import Network.URI
import Text.JSON (showJSON)

import Network.Anarchy

resultToMaybeT :: Monad m => Result a -> MaybeT m a
resultToMaybeT (Right x) = return x
resultToMaybeT _ =  MaybeT (return Nothing)

register :: HostPort -> HostPort -> MaybeT IO ()
register hp myhp = do
  let HostPort h p = hp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/register" "" ""
  let request = Request uri PUT [] (show . showJSON $ myhp)
  result <- lift . simpleHTTP $ request
  response <- resultToMaybeT result
  return ()

hostcheck :: HostPort -> MaybeT IO String
hostcheck hp = do
  let HostPort h p = hp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/hostcheck" "" ""
  let request = Request uri GET [] ""
  result <- lift . simpleHTTP $ request
  response <- resultToMaybeT result
  return $ rspBody response
