module Main (main) where

import           Control.Monad
import           Network
import           Reserve

main :: IO ()
main = withSocketsDo $ withSession (forever . run)
