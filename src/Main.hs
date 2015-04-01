module Main (main) where

import           Prelude.Compat

import           Network

import           Options
import           Reserve

main :: IO ()
main = withSocketsDo $ do
  withOptions $ \opts -> do
    putStrLn $ "http://localhost:" ++ show (optionsReservePort opts)
    run opts
