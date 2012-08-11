import Happstack.Lite
import Happstack.Server (Request(..), askRq)

import Network.Anarchy
import Network.Anarchy.Client
import Network.Anarchy.Server

import System.Console.GetOpt
import System.Environment

import Data.String.Utils

data Flag = Port Int | Bootstrap String

options :: [OptDescr Flag]
options = [ Option ['p'] ["Port"] (ReqArg (Port . read) "8080")  "port number",
            Option ['b'] ["Bootstrap"] (ReqArg Bootstrap "")  "bootstrap" ]

findPort :: [ Flag ] -> Int
findPort (a:as) = case a of
  Port i -> i
  _ -> findPort as

findBootstrap :: [ Flag ] -> HostPort
findBootstrap (a:as) = case a of
  Bootstrap s -> let host:port:[] = split ":" s
                 in HostPort host (read port)

main = do
  putStrLn "Anarchy."
  args <- getArgs
  let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
  let serverConfig = defaultServerConfig { Happstack.Lite.port = findPort flags }
  h <- runServer serverConfig (HostPort "hink.es" 8080)
  block h
