import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import Happstack.Lite
import System.Timeout

myApp :: MVar Integer -> ServerPart Response
myApp x = msum [
  dir "get" $ get x,
  dir "start" $ start x
  ]

incrementLoop :: MVar Integer -> IO ()
incrementLoop x = do
  val <- readMVar x
  swapMVar x (val+1)
  threadDelay 1000000
  incrementLoop x

start :: MVar Integer -> ServerPart Response
start x = do
  liftIO $ forkIO $ do
    timeout 5000000 $ incrementLoop x
    return ()
  return $ toResponse "Started"

get :: ToMessage a => MVar a -> ServerPart Response
get x = do
  val <- liftIO $ readMVar x
  return $ toResponse val

main = do
  putStrLn "Anarchy."
  x <- newMVar (0 :: Integer)
  serve Nothing $ myApp x
