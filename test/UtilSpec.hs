{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (main, spec) where

import           Test.Hspec
import           System.IO
import           Control.Exception
import           Control.Concurrent
import           Network.Socket
import           Network.Socket.ByteString (sendAll)
import           Data.Streaming.Network (bindPortTCP)

import           Util

main :: IO ()
main = hspec spec

startTestServer :: IO ()
startTestServer = bracket (bindPortTCP 6060 "*") close $ \s -> do
  (sock, _) <- accept s
  sendAll sock "foo"
  close sock

withTestServer :: Int -> IO a -> IO a
withTestServer delay = bracket (forkIO $ threadDelay delay >> startTestServer) killThread . const

spec :: Spec
spec = do
  describe "connectRetry" $ do
    it "connects to a TCP port" $ do
      withTestServer 0 $ do
        connectRetry 5000 "localhost" 6060 $ \(Just h) -> do
          hGetContents h `shouldReturn` "foo"

    context "when server socket is not yet open" $ do
      it "retries" $ do
        withTestServer 20000 $ do
          connectRetry 5000 "localhost" 6060 $ \(Just h) -> do
            hGetContents h `shouldReturn` "foo"

      context "after 10 retries" $ do
        it "gives up" $ do
          withTestServer 20000 $ do
            connectRetry 500 "localhost" 6060 (`shouldBe` Nothing)
