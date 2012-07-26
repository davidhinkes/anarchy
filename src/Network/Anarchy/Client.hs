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

resultToMaybeT :: Monad m => Result a -> MaybeT m a
resultToMaybeT (Right x) = return x
resultToMaybeT _ =  MaybeT (return Nothing)

register :: String -> Int -> MaybeT IO String
register h p = do
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) ("/register/" ++ (show p)) "" ""
  let request = Request uri PUT [] ""
  result <- lift . simpleHTTP $ request
  response <- resultToMaybeT result
  return $ rspBody response

hostcheck :: String -> Int -> MaybeT IO String
hostcheck h p = do
  let uriAuth = URIAuth "" h (":" ++ (show p))
  let uri = URI "http:" (Just uriAuth) ("/up/" ++ (show p)) "" ""
  let request = Request uri GET [] ""
  result <- lift . simpleHTTP $ request
  response <- resultToMaybeT result
  return $ rspBody response
