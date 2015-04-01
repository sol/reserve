{-# LANGUAGE CPP #-}
module Interpreter (
  Interpreter
, new
, terminate
, start
, stop
, reload
) where

import           Prelude.Compat

import           System.Process
import           System.Process.Internals
import           System.IO
import           System.Posix.Signals hiding (signalProcess)
import qualified System.Posix.Signals as Posix
import           Control.Concurrent

data Interpreter = Interpreter ProcessHandle Handle

new :: FilePath -> IO Interpreter
new src = do
  (Just hIn, Nothing, Nothing, processHandle) <- createProcess $ (proc "ghci" ["-v0", src]) {std_in = CreatePipe}
  return (Interpreter processHandle hIn)

terminate :: Interpreter -> IO ()
terminate i@(Interpreter p h) = stop i >> hClose h >> waitForProcess p >> return ()

start :: Interpreter -> [String] -> IO ()
start (Interpreter _ h) args = hPutStrLn h (unwords $ ":main" : args) >> hFlush h

stop :: Interpreter -> IO ()
stop (Interpreter p _) = signalProcess sigINT p

reload :: Interpreter -> IO ()
reload (Interpreter _ h) = hPutStrLn h ":reload" >> hFlush h

signalProcess :: Signal -> ProcessHandle -> IO ()
#if MIN_VERSION_process(1,2,0)
signalProcess signal (ProcessHandle mvar _) =
#else
signalProcess signal (ProcessHandle mvar) =
#endif
  withMVar mvar $ \p -> case p of
    OpenHandle pid -> Posix.signalProcess signal pid
    _ -> return ()
