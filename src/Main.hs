module Main (main) where

import           Control.Exception
import           Network
import qualified Reserve

main :: IO ()
main = withSocketsDo $ bracket (listenOn $ PortNumber 4040) sClose Reserve.run
