## [re](https://github.com/sol/reserve#readme)serve

**DISCLAIMER:** This is very early stage software.  Consider it as a proof of concept that is not meant for production use!

### Requirements

Currently, reserve assumes that your application:

 * listens on port `8080`
 * works with `ghci`
 * dose not require any command-line arguments

(some of those restriction may be lifted in a future version)

### Example

```haskell
-- app.hs
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "hello"

main :: IO ()
main = run 8080 app
```

```
$ reserve app.hs
```

Make requests to <http://localhost:4040>.
