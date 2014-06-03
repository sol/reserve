{-# LANGUAGE OverloadedStrings #-}
module ReserveSpec (main, spec) where

import           Test.Hspec

import           Control.Exception
import           Control.Concurrent
import           System.Directory
import           Network.HTTP.Conduit

import           Reserve

main :: IO ()
main = hspec spec

withServer :: IO () -> IO ()
withServer action = do
  mvar <- newEmptyMVar
  bracket (runReserve mvar) killThread (const action)
  takeMVar mvar
  where
    runReserve mvar = forkIO $ run "test/resources/hello.hs" `finally` putMVar mvar ()

spec :: Spec
spec = around withServer $ do
  describe "run" $ do
    it "runs app" $ do
      simpleHttp "http://localhost:4040/" `shouldReturn` "hello"

    it "reloads app" $ do
      simpleHttp "http://localhost:4040/" `shouldReturn` "hello"
      withModifiedApp $ simpleHttp "http://localhost:4040/" `shouldReturn` "foo"
  where
    withModifiedApp = bracket_
      (renameFile "test/resources/hello.hs" "test/resources/hello.hs.bak")
      (renameFile "test/resources/hello.hs.bak" "test/resources/hello.hs")
      . (copyFile "test/resources/hello_mod.hs" "test/resources/hello.hs" >>)
