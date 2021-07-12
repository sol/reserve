module Main (main) where

import           Network.Socket

import           Options
import           Reserve

main :: IO ()
main = withSocketsDo $ do
  withOptions $ \opts -> do
    putStrLn $ "http://localhost:" ++ show (optionsReservePort opts)
    run opts
