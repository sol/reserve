module OptionsSpec (main, spec) where

import           Prelude.Compat

import           Test.Hspec

import           Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseOptions" $ do
    it "parses port" $ do
      parseOptions ["--port", "8000"] `shouldBe` Right defaultOptions {optionsPort = 8000}

    it "allows to specify path to Main module" $ do
      parseOptions ["Foo.hs"] `shouldBe` Right defaultOptions {optionsMainIs = "Foo.hs"}

    it "allows to specify addition arguments for app" $ do
      parseOptions ["--port", "8000", "--", "production", "--port", "3000"] `shouldBe` Right defaultOptions {optionsPort = 8000, optionsAppArgs = ["production", "--port", "3000"]}
