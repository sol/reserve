## [re](https://github.com/sol/reserve#readme)serve

**DISCLAIMER:** This is early stage software.  It is already functional and
useful, but it still has some rough edges.

### Requirements

Currently, reserve assumes that your application:

 * works with `ghci`
 * dose not require any command-line arguments

(some of those restriction may be lifted in a future version)

### Example

~~~ {.haskell}
-- app.hs
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] "hello"

main :: IO ()
main = run 3000 app
~~~

```
$ reserve app.hs
```

Make requests to <http://localhost:4040>.
