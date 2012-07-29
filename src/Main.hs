import Happstack.Lite
import Happstack.Server (Request(..), askRq)

import Network.Anarchy
import Network.Anarchy.Client
import Network.Anarchy.Server

main = do
  putStrLn "Anarchy."
  let serverConfig = defaultServerConfig { Happstack.Lite.port = 8080 }
  h <- runServer serverConfig (HostPort "hink.es" 8080)
  block h
