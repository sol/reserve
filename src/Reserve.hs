{-# LANGUAGE OverloadedStrings #-}
module Reserve (run) where

import           Prelude.Compat

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.IO.Exception
import           System.IO

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Network

import           Network.HTTP.Toolkit
import           Network.HTTP.Types

import           Interpreter
import           Options
import           Util

data Session = Session Socket

openSession :: Options -> IO Session
openSession opts = Session <$> listenOn (PortNumber $ optionsReservePort opts)

closeSession :: Session -> IO ()
closeSession (Session h) = sClose h

withSession :: Options -> (Session -> Interpreter.InterpreterM a) -> IO a
withSession opts action =
  bracket (openSession opts) closeSession $ \ session ->
    withInterpreter (optionsMainIs opts) $
      action session

run :: Options -> IO ()
run opts = withSession opts $ \(Session s) -> forever $ do
  (h, _, _) <- liftIO $ accept s
  Interpreter.reload
  Interpreter.start (optionsAppArgs opts)
  c <- liftIO $ inputStreamFromHandle h
  let send :: ByteString -> IO ()
      send = ignoreResourceVanished . B.hPutStr h
  liftIO $ readRequest True c >>= httpRequest (optionsPort opts) send
  Interpreter.stop
  liftIO $ ignoreResourceVanished $ hClose h

ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished action = catchJust (guard . (== ResourceVanished) . ioe_type) action return

gatewayTimeout :: (ByteString -> IO ()) -> IO ()
gatewayTimeout send = simpleResponse send gatewayTimeout504 headers "timeout"
  where
    headers = [("Content-Type", "text/plain"), ("Connection", "close")]

httpRequest :: PortNumber -> (ByteString -> IO ()) -> Request BodyReader -> IO ()
httpRequest port send request@(Request method _ headers _) = connectRetry 200000 "localhost" port $ \mh -> case mh of
  Just h -> do
    sendRequest (B.hPutStr h) request{requestHeaders = setConnectionClose headers}
    inputStreamFromHandle h >>= readResponse True method >>= sendResponse send
  Nothing -> gatewayTimeout send
  where
    setConnectionClose = (("Connection", "close") :) . filter ((/= "Connection") . fst)
