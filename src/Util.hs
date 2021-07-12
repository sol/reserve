{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import           Control.Concurrent
import           Control.Exception
import           System.IO
import           Network.Socket (HostName, socketToHandle)
import qualified Data.ByteString.Char8 as B
import           Data.Streaming.Network

import           Options (Port)

connectRetry :: Int -> HostName -> Port -> (Maybe Handle -> IO a) -> IO a
connectRetry delay host port action = go 0
  where
    go n
      | n < (10 :: Int) = do
        tryConnect >>= either (\(_ :: IOException) -> retry n) return
      | otherwise = action Nothing

    tryConnect = try $ bracket connect hClose (action . Just)
    retry n = threadDelay delay >> go (succ n)
    connect = do
      (sock, _) <- getSocketTCP (B.pack host) port
      socketToHandle sock ReadWriteMode
