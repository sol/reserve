module Main (main) where

import           System.Environment
import           System.Exit.Compat
import           Network
import           Reserve

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of
    [src] -> do
      putStrLn "http://localhost:4040"
      run src
    _ -> die "usage: reserve <path>"
