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
import Text.JSON (encode)

import Network.Anarchy

resultToMaybeT :: Monad m => Result a -> MaybeT m a
resultToMaybeT (Right x) = return x
resultToMaybeT _ =  MaybeT (return Nothing)

register :: HostPort -> UniqueMessage HostPort -> IO ()
register tohp msg = do
  let HostPort h p = tohp
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) "/register" "" ""
  let request = mkRequest PUT uri
  let request' = setRequestBody request ("text/json", encode msg)
  response <-simpleHTTP $ request'
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
