{-# LANGUAGE OverloadedStrings #-}
module ReserveSpec (main, spec) where

import           Test.Hspec

import           Control.Exception
import           Control.Concurrent
import           Network.HTTP.Conduit

import           Reserve

main :: IO ()
main = hspec spec

withServer :: IO () -> IO ()
withServer action = withSession $ \s -> do
  mvar <- newEmptyMVar
  _ <- forkIO (run s `finally` putMVar mvar ())
  action
  takeMVar mvar

spec :: Spec
spec = around withServer $ do
  describe "run" $ do
    it "runs a server app" $ do
      simpleHttp "http://localhost:4040/" `shouldReturn` "hello"
