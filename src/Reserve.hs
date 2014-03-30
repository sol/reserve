module Reserve (run) where

import           Data.Char
import           System.IO
import           Network

import           Control.DeepSeq
import           Control.Exception

run :: Socket -> IO ()
run s = do
  (h, _, _) <- accept s
  getHeader h >>= httpRequest >>= hPutStr h

getHeader :: Handle -> IO [String]
getHeader h = go []
  where
    go xs = do
      x <- hGetLine h
      case dropWhile isSpace x of
        "" -> return $ reverse xs
        _ -> go (x : xs)

httpRequest :: [String] -> IO String
httpRequest req = do
  bracket (connectTo "localhost" $ PortNumber 8080) hClose $ \h -> do
    mapM_ (hPutStrLn h) req
    hPutStr h "Connection: close\r\n"
    hPutStr h "\r\n"
    hGetContents h >>= evaluate . force
