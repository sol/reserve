module UtilSpec (main, spec) where

import           Prelude.Compat

import           Test.Hspec
import           System.IO
import           Control.Exception
import           Control.Concurrent
import           Network

import           Util

main :: IO ()
main = hspec spec

startTestServer :: IO ()
startTestServer = bracket (listenOn $ PortNumber 6060) sClose $ \s -> do
  (h, _, _) <- accept s
  hPutStr h "foo"
  hClose h

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
