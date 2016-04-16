{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter (
  InterpreterM
, withInterpreter
, start
, stop
, reload
) where

import           Prelude.Compat

import           Control.Concurrent
import           Control.Exception (AsyncException(UserInterrupt))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Language.Haskell.Interpreter hiding (get)
import           System.Environment
import           System.Exit

type InterpreterM
  = InterpreterT (StateT (Maybe ThreadId) (ReaderT FilePath IO))

withInterpreter :: FilePath -> InterpreterM a -> IO a
withInterpreter src action = do
  r <- runReaderT (evalStateT (runInterpreter (reload >> action)) Nothing) src
  case r of
    Right a -> return a
    Left err -> die $ show err

start :: [String] -> InterpreterM ()
start args = do
  action <- interpret "Main.main" (return () :: IO ())
  thread <- liftIO $ forkIO $
    withArgs args action
  lift $ put $ Just thread

stop :: InterpreterM ()
stop = do
  mThread <- lift get
  forM_ mThread $ \ thread ->
    liftIO $ throwTo thread UserInterrupt
  lift $ put Nothing

reload :: InterpreterM ()
reload = do
  src <- lift ask
  loadModules [src]
  loaded <- getLoadedModules
  setTopLevelModules (loaded)
