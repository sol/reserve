module Interpreter (
  Interpreter
, new
, terminate
, start
, stop
, reload
) where

import           System.Process
import           System.Process.Internals
import           System.IO
import           System.Posix.Signals hiding (signalProcess)
import qualified System.Posix.Signals as Posix
import           Control.Concurrent

data Interpreter = Interpreter ProcessHandle Handle

new :: IO Interpreter
new = do
  (Just hIn, Nothing, Nothing, processHandle) <- createProcess $ (proc "ghci" ["-v0", "hello.hs"]) {std_in = CreatePipe}
  return (Interpreter processHandle hIn)

terminate :: Interpreter -> IO ()
terminate i@(Interpreter p h) = stop i >> hClose h >> waitForProcess p >> return ()

start :: Interpreter -> IO ()
start (Interpreter _ h) = hPutStrLn h ":main" >> hFlush h

stop :: Interpreter -> IO ()
stop (Interpreter p _) = signalProcess sigINT p

reload :: Interpreter -> IO ()
reload (Interpreter _ h) = hPutStrLn h ":reload" >> hFlush h

signalProcess :: Signal -> ProcessHandle -> IO ()
signalProcess signal (ProcessHandle mvar) = withMVar mvar $ \p -> case p of
  OpenHandle pid -> Posix.signalProcess signal pid
  _ -> return ()
