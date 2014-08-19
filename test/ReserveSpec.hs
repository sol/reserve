{-# LANGUAGE OverloadedStrings #-}
module ReserveSpec (main, spec) where

import           Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as L
import           Control.Exception
import           Control.Concurrent
import           System.IO
import           System.Directory
import           Network
import           Network.HTTP.Conduit

import           Options
import           Reserve

main :: IO ()
main = hspec spec

withServer :: IO () -> IO ()
withServer action = do
  mvar <- newEmptyMVar
  bracket (runReserve mvar) killThread (const $ yield >> action)
  takeMVar mvar
  where
    runReserve mvar = forkIO $ run defaultOptions {optionsMainIs = "test/resources/hello.hs"} `finally` putMVar mvar ()

spec :: Spec
spec = around withServer $ do
  describe "run" $ do
    it "runs app" $ do
      simpleHttp "http://localhost:12000/" `shouldReturn` "hello"

    it "reloads app" $ do
      simpleHttp "http://localhost:12000/" `shouldReturn` "hello"
      withModifiedApp $ simpleHttp "http://localhost:12000/" `shouldReturn` "foo"

    it "can deal with large response bodies" $ do
      simpleHttp "http://localhost:12000/large-response" `shouldReturn` (L.take 100000 $ L.cycle "foo bar baz\n")

    context "when client closes connection early" $ do
      it "ignores that client" $ do
        h <- connectTo "localhost" (PortNumber 12000)
        hPutStr h "GET / HTTP/1.1\r\n\r\n"
        hClose h
        simpleHttp "http://localhost:12000/" `shouldReturn` "hello"
  where
    withModifiedApp = bracket_
      (renameFile "test/resources/hello.hs" "test/resources/hello.hs.bak")
      (renameFile "test/resources/hello.hs.bak" "test/resources/hello.hs")
      . (copyFile "test/resources/hello_mod.hs" "test/resources/hello.hs" >>)
