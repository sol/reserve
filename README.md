# [re](https://github.com/sol/reserve#readme)serve
**DISCLAIMER:** This is early stage software.  It is already functional and
useful, but it still has some rough edges.

`reserve` provides code reloading for Haskell web applications.  You can run
you application with `reserve` during development and code changes will take
immediate effect.

## Requirements

`reserve` can reload arbitrary Haskell web applications.  The only requirement
is that your application works with `ghci`.

## Examples

### Scotty

Create a file `app.hs` with the following content:

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    text "hello"
~~~

```
$ reserve app.hs
```

Make a request to <http://localhost:12000>, modify `app.hs`, reload!

### Snap (sandboxed)

```
$ mkdir my-project && cd my-project
$ cabal sandbox init
$ cabal install snap
$ cabal exec snap init barebones
$ cabal exec -- reserve -p 8000
```

Make a request to <http://localhost:12000>, modify `src/Main.hs`, reload!

## Customization

By default `reserve` assumes that the `Main` module of your application is at
`src/Main.hs`.  You can customize this by passing the path to your `Main`
module to `reserve`:

```
$ reserve src/app.hs
```

By default `reserve` assumes that your application listens on port `3000`.  You
can customize this by passing `--port` to `reserve`:

```
$ reserve --port 8000
```

By default `reserve` serves your application on port `12000`.  You can
customize this by passing `--reserve-port` to `reserve`:

```
$ reserve --reserve-port 4040
```

You can pass command-line arguments to your application by separating them with
`--`:

```
$ reserve src/app.hs -- --mode development
```

`reserve` uses `ghci` to run your application.  If your application requires
any addition GHC options, you can put them into `./.ghci`:

```
$ echo ":set -isrc" >> .ghci
$ reserve src/app.hs
```

`reserve` works with Cabal sandboxes, just run it with `cabal exec -- reserve`.
