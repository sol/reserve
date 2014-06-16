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
import           Interpreter (Interpreter)
import qualified Interpreter

data Session = Session Socket Interpreter

openSession :: String -> IO Session
openSession src = Session <$> (listenOn $ PortNumber 4040) <*> Interpreter.new src

closeSession :: Session -> IO ()
closeSession (Session h i) = sClose h >> Interpreter.terminate i

withSession :: String -> (Session -> IO a) -> IO a
withSession src = bracket (openSession src) closeSession

run :: FilePath -> IO ()
run src = withSession src $ \(Session s int) -> forever $ do
  (h, _, _) <- accept s
  Interpreter.reload int
  Interpreter.start int
  c <- inputStreamFromHandle h
  let send = ignoreResourceVanished . B.hPutStr h
  readRequest True c >>= httpRequest >>= maybe (gatewayTimeout send) (sendResponse send)
  Interpreter.stop int
  ignoreResourceVanished $ hClose h

ignoreResourceVanished :: IO () -> IO ()
ignoreResourceVanished action = catchJust (guard . (== ResourceVanished) . ioe_type) action return

gatewayTimeout :: (ByteString -> IO ()) -> IO ()
gatewayTimeout send = simpleResponse send gatewayTimeout504 headers "timeout"
  where
    headers = [("Content-Type", "text/plain"), ("Connection", "close")]

httpRequest :: Request BodyReader -> IO (Maybe (Response BodyReader))
httpRequest request@(Request method _ headers _) = connectRetry 200000 "localhost" 8080 $ \mh -> case mh of
  Just h -> do
    sendRequest (B.hPutStr h) request{requestHeaders = setConnectionClose headers}
    c <- inputStreamFromHandle h
    Just <$> readResponse True method c
  Nothing -> return Nothing
  where
    setConnectionClose = (("Connection", "close") :) . filter ((/= "Connection") . fst)
