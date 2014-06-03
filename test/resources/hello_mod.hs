{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "foo"

main :: IO ()
main = run 8080 app
