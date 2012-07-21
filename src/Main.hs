import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Happstack.Lite
import System.Timeout
import Text.JSON

data State = State {
  clients :: [ B.ByteString ]
}

stateToJ :: State -> JSValue
stateToJ s = showJSON . clients $ s

instance ToMessage State where
  toMessage s =
    let clientsJs = JSObject $ toJSObject $ [ ("clients", showJSON . clients $ s) ]
    in LB.pack . encode $ clientsJs
  toContentType _ = B.pack "text/json"

myApp :: MVar State -> ServerPart Response
myApp x = msum [
  dir "status" $ status x
  ]

status :: MVar State -> ServerPart Response
status x = do
  val <- liftIO . readMVar $ x
  return . toResponse $ val

main = do
  putStrLn "Anarchy."
  x <- newMVar (State [])
  serve Nothing $ myApp x
