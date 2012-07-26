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
import Network.Anarchy.Client
import Network.Anarchy.Server

main = do
  putStrLn "Anarchy."
  let serverConfig = defaultServerConfig { Happstack.Lite.port = 8080 }
  runServer serverConfig (HostPort "hink.es" 8080)
