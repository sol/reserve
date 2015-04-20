module Helper (
  module Test.Hspec
, module Control.Applicative
, inTempDirectory
) where

import           Test.Hspec
import           Prelude.Compat
import           Control.Applicative
import           Control.Exception
import           System.Directory
import           System.IO.Temp

inTempDirectory :: IO a -> IO a
inTempDirectory action = withSystemTempDirectory "hspec" $ \path -> do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory path
    action
