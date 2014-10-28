{-# LANGUAGE OverloadedStrings #-}
module Reserve (run) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           GHC.IO.Exception
import           System.IO

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Network

import           Network.HTTP.Types
import           Network.HTTP.Toolkit

import           Util
import           Options
import           Interpreter (Interpreter)
import qualified Interpreter
import           Data.IORef

data Session = Session Socket Interpreter

openSession :: Options -> IO Session
-- openSession opts = Session <$> listenOn (optionsReservePort opts) <*> Interpreter.new (optionsMainIs opts)
openSession opts = Session <$> listenOn (PortNumber $ optionsReservePort opts) <*> Interpreter.new (optionsMainIs opts)

closeSession :: Session -> IO ()
closeSession (Session h i) = sClose h >> Interpreter.terminate i

withSession :: Options -> (Session -> IO a) -> IO a
withSession opts = bracket (openSession opts) closeSession

run :: Options -> IO ()
run opts = withSession opts $ \(Session s int) -> do
  Interpreter.start int (optionsAppArgs opts)
  ref <- getModTime >>= newIORef
  forever $ do
    (h, _, _) <- accept s
    t0 <- readIORef ref
    t1 <- getModTime
    when (t0 < t1) $ do
      Interpreter.stop int
      Interpreter.reload int
      Interpreter.start int (optionsAppArgs opts)
      writeIORef ref t1
    c <- inputStreamFromHandle h
    let send :: ByteString -> IO ()
        send = ignoreResourceVanished . B.hPutStr h
    readRequest True c >>= httpRequest (optionsPort opts) send
    ignoreResourceVanished $ hClose h

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
