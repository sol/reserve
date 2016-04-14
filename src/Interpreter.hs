{-# LANGUAGE CPP #-}
module Interpreter (
  Interpreter
, InterpreterM
, withInterpreter
, start
, stop
, reload
) where

import           Prelude.Compat

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           System.IO
import qualified System.Posix.Signals as Posix
import           System.Posix.Signals hiding (signalProcess)
import           System.Process
import           System.Process.Internals

data Interpreter = Interpreter ProcessHandle Handle

new :: FilePath -> IO Interpreter
new src = do
  (Just hIn, Nothing, Nothing, processHandle) <- createProcess $ (proc "ghci" ["-v0", src]) {std_in = CreatePipe}
  return (Interpreter processHandle hIn)

terminate :: Interpreter -> IO ()
terminate i@(Interpreter p h) = runReaderT stop i >> hClose h >> waitForProcess p >> return ()

type InterpreterM = ReaderT Interpreter IO

withInterpreter :: FilePath -> InterpreterM a -> IO a
withInterpreter src action = bracket (new src) terminate $ \ interpreter ->
  runReaderT action interpreter

start :: [String] -> InterpreterM ()
start args = do
  Interpreter _ h <- ask
  liftIO $ hPutStrLn h (unwords $ ":main" : args) >> hFlush h

stop :: InterpreterM ()
stop = do
  Interpreter p _ <- ask
  liftIO $ signalProcess sigINT p

reload :: InterpreterM ()
reload = do
  Interpreter _ h <- ask
  liftIO $ hPutStrLn h ":reload" >> hFlush h

signalProcess :: Signal -> ProcessHandle -> IO ()
#if MIN_VERSION_process(1,2,0)
signalProcess signal (ProcessHandle mvar _) =
#else
signalProcess signal (ProcessHandle mvar) =
#endif
  withMVar mvar $ \p -> case p of
    OpenHandle pid -> Posix.signalProcess signal pid
    _ -> return ()
