module Reserve (
  Session
, withSession
, run
) where

import           Control.Applicative
import           Data.Char
import           System.IO
import           Network

import           Control.DeepSeq
import           Control.Exception

import           Util
import           Interpreter (Interpreter)
import qualified Interpreter

data Session = Session Socket Interpreter

openSession :: IO Session
openSession = Session <$> (listenOn $ PortNumber 4040) <*> Interpreter.new

closeSession :: Session -> IO ()
closeSession (Session h i) = sClose h >> Interpreter.terminate i

withSession :: (Session -> IO a) -> IO a
withSession = bracket openSession closeSession

run :: Session -> IO ()
run (Session s int) = do
  (h, _, _) <- accept s
  Interpreter.reload int
  Interpreter.start int
  getHeader h >>= httpRequest >>= maybe (gatewayTimeout h) (hPutStr h)
  Interpreter.stop int
  where
    gatewayTimeout h = do
      hPutStr h "HTTP/1.1 504 Gateway Timeout\r\n"
      hPutStr h "Content-Type: text/plain\r\n"
      hPutStr h "Connection: close\r\n"
      hPutStr h "Content-Length: 7\r\n"
      hPutStr h "\r\n"
      hPutStr h "timeout"

getHeader :: Handle -> IO [String]
getHeader h = go []
  where
    go xs = do
      x <- hGetLine h
      case dropWhile isSpace x of
        "" -> return $ reverse xs
        _ -> go (x : xs)

httpRequest :: [String] -> IO (Maybe String)
httpRequest req = connectRetry 100000 "localhost" 8080 $ \mh -> case mh of
  Just h -> do
    mapM_ (hPutStrLn h) req
    hPutStr h "Connection: close\r\n"
    hPutStr h "\r\n"
    hGetContents h >>= evaluate . force . Just
  Nothing -> return Nothing
