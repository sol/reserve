{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Reserve (run) where

import           Control.Monad
import           Control.Exception
import           GHC.IO.Exception
import           System.IO

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Network.Socket
import           Data.Streaming.Network (bindPortTCP)

import           Network.HTTP.Types
import           Network.HTTP.Toolkit

import           Util
import           Options
import           Interpreter (Interpreter)
import qualified Interpreter

data Session = Session Socket Interpreter

openSession :: Options -> IO Session
openSession opts = Session <$> bindPortTCP (optionsReservePort opts) "*" <*> Interpreter.new (optionsMainIs opts)

closeSession :: Session -> IO ()
closeSession (Session h i) = close h >> Interpreter.terminate i

withSession :: Options -> (Session -> IO a) -> IO a
withSession opts = bracket (openSession opts) closeSession

run :: Options -> IO ()
run opts = withSession opts $ \(Session s interpreter) -> forever $ do
  (sock, _) <- accept s
  Interpreter.reload interpreter
  Interpreter.start interpreter (optionsAppArgs opts)
  h <- socketToHandle sock ReadWriteMode
  c <- inputStreamFromHandle h
  let send :: ByteString -> IO ()
      send = ignoreResourceVanished . B.hPutStr h
  readRequest True c >>= httpRequest (optionsPort opts) send
  Interpreter.stop interpreter
  ignoreResourceVanished $ hClose h

ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished action = catchJust (guard . (== ResourceVanished) . ioe_type) action return

gatewayTimeout :: (ByteString -> IO ()) -> IO ()
gatewayTimeout send = simpleResponse send gatewayTimeout504 headers "timeout"
  where
    headers = [("Content-Type", "text/plain"), ("Connection", "close")]

httpRequest :: Port -> (ByteString -> IO ()) -> Request BodyReader -> IO ()
httpRequest port send request@(Request method _ headers _) = connectRetry 200000 "localhost" port $ \ case
  Just h -> do
    sendRequest (B.hPutStr h) request{requestHeaders = setConnectionClose headers}
    inputStreamFromHandle h >>= readResponse True method >>= sendResponse send
  Nothing -> gatewayTimeout send
  where
    setConnectionClose = (("Connection", "close") :) . filter ((/= "Connection") . fst)
