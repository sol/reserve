module Main (main) where

import           Control.Monad
import           System.Environment
import           System.Exit.Compat
import           Network
import           Reserve

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> do
      putStrLn "http://localhost:4040"
      withSocketsDo $ withSession src (forever . run)
    _ -> die "usage: reserve <path>"
