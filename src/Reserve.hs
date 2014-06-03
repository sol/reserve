{-# LANGUAGE OverloadedStrings #-}
module Reserve (run) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
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
  c <- connectionFromHandle h
  let send = B.hPutStr h
  readRequest c >>= httpRequest >>= maybe (gatewayTimeout send) (sendResponse send)
  Interpreter.stop int
  hClose h

gatewayTimeout :: (ByteString -> IO ()) -> IO ()
gatewayTimeout send = simpleResponse send gatewayTimeout504 headers "timeout"
  where
    headers = [("Content-Type", "text/plain"), ("Connection", "close")]

httpRequest :: Request BodyReader -> IO (Maybe (Response BodyReader))
httpRequest request@(Request method _ headers _) = connectRetry 100000 "localhost" 8080 $ \mh -> case mh of
  Just h -> do
    sendRequest (B.hPutStr h) request{requestHeaders = setConnectionClose headers}
    c <- connectionFromHandle h
    Just <$> readResponse method c
  Nothing -> return Nothing
  where
    setConnectionClose = (("Connection", "close") :) . filter ((/= "Connection") . fst)
