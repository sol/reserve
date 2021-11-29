{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ReserveSpec (spec) where

import           Helper

import qualified Data.ByteString.Lazy.Char8 as L
import           Control.Exception
import           Control.Concurrent
import           Network.Socket
import           Network.Socket.ByteString (sendAll)
import           Data.Streaming.Network (getSocketTCP)
import           Network.HTTP.Conduit
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import           Options
import           Reserve

appWithResponse :: String -> IO ()
appWithResponse response = writeFile "app.hs" $ unindent [i|
  {-# LANGUAGE OverloadedStrings #-}
  module Main (main) where

  import Network.Wai
  import Network.HTTP.Types
  import Network.Wai.Handler.Warp (run)
  import qualified Data.ByteString.Lazy.Char8 as B

  app :: Application
  app _ = ($ responseLBS status200 [("Content-Type", "text/plain")] #{response})

  main :: IO ()
  main = run 3000 app
|]

literal :: String -> String
literal = show

withServer :: IO () -> IO ()
withServer action = inTempDirectory $ do
  appWithResponse (literal "hello")
  mvar <- newEmptyMVar
  bracket (runReserve mvar) killThread (const $ yield >> action)
  takeMVar mvar
  where
    runReserve mvar = forkIO $ run defaultOptions {optionsMainIs = "app.hs"} `finally` putMVar mvar ()

spec :: Spec
spec = around_ withServer $ do
  describe "run" $ do
    it "runs app" $ do
      simpleHttp "http://localhost:12000/" `shouldReturn` "hello"

    it "reloads app" $ do
      simpleHttp "http://localhost:12000/" `shouldReturn` "hello"
      appWithResponse (literal "foo")
      simpleHttp "http://localhost:12000/" `shouldReturn` "foo"

    it "can deal with large response bodies" $ do
      appWithResponse [i|(B.take 100000 $ B.cycle #{literal "foo bar baz\n"})|]
      simpleHttp "http://localhost:12000/large-response" `shouldReturn` (L.take 100000 $ L.cycle "foo bar baz\n")

    context "when client closes connection early" $ do
      it "ignores that client" $ do
        (sock, _) <- getSocketTCP "localhost" 12000
        sendAll sock "GET / HTTP/1.1\r\n\r\n"
        close sock
        simpleHttp "http://localhost:12000/" `shouldReturn` "hello"
