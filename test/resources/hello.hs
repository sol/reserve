{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as B
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app request respond = respond . responseLBS status200 [("Content-Type", "text/plain")] $ case pathInfo request of
  ["large-response"] -> B.take 100000 $ B.cycle "foo bar baz\n"
  _ -> "hello"

main :: IO ()
main = run 8080 app
