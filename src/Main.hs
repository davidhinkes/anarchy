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

findPort :: [ Flag ] -> Int -> Int
findPort (a:as) d = case a of
  Port i -> i
  _ -> findPort as d
findPort [] d = d

findBootstrap :: [ Flag ] -> HostPort-> HostPort
findBootstrap (a:as) d = case a of
  Bootstrap s -> let host:port:[] = split ":" s
                 in HostPort host (read port)
  _ -> findBootstrap as d
findBootstrap [] d = d

main = do
  putStrLn "Anarchy."
  args <- getArgs
  let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
  let serverConfig = defaultServerConfig { Happstack.Lite.port = findPort flags 8080 }
  h <- runServer serverConfig (findBootstrap flags (HostPort "hink.es" 8080))
  block h
