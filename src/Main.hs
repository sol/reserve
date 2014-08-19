module Main (main) where

import           Network

import           Options
import           Reserve

main :: IO ()
main = withSocketsDo $ do
  withOptions $ \opts -> do
    putStrLn "http://localhost:4040"
    run opts
