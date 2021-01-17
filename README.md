# wai-cli [![Hackage](https://img.shields.io/hackage/v/wai-cli.svg?style=flat)](https://hackage.haskell.org/package/wai-cli) [![Build Status](https://github.com/myfreeweb/wai-cli/workflows/WaiCLI%20CI/badge.svg)](https://github.com/myfreeweb/wai-cli/actions?query=workflow%3A%22WaiCLI+CI%22) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A command line runner for Wai apps (using Warp) with support for:

- `--protocol http --port 8000` TCP sockets
- `--protocol unix --socket /var/run/app/sock` UNIX domain sockets
- `--protocol cgi` running as a CGI app (the original serverless lambda functions from the 90s)
- `--protocol fastcgi` running as a FastCGI app (OPTIONAL! You need to build with the `fastcgi` cabal flag and you need to have `libfcgi` (e.g. `fcgi-devkit` package on FreeBSD) installed)
- `--protocol activate` socket activation (systemd-compatible, but not restricted to systemd in any way. see [soad](https://github.com/myfreeweb/soad) for an interesting use of (de)activation!)
- `--protocol (http+tls|unix+tls|activate+tls) --tlskey key.pem --tlscert cert.pem` TLS (can be turned off with a cabal flag to avoid compiling [the TLS library](https://github.com/vincenthz/hs-tls))
- `--graceful (none|serve-normally|serve-503)` graceful shutdown (on TERM signal)
- `--devlogging` development logging (from `wai-extra`)
- printing a pretty and colorful run message (e.g. `Running on http port 3000 with 4 CPUs`) (you can replace it with your own, or with nothing)

Extracted from [sweetroll](https://github.com/myfreeweb/sweetroll) and [microformats2-parser](https://github.com/myfreeweb/microformats2-parser)'s demo web app.

Now used in the [magicbane](https://github.com/myfreeweb/magicbane) framework (which was also extracted from sweetroll).

## Usage

Add a dependency on `wai-cli` and write something like this:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Cli
import Data.Monoid (mconcat)

app = scottyApp $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

main = defWaiMain =<< app
```

Want to use command line args for your application-specific settings? Don't.
Use environment variables instead.
Possibly with [dotenv](https://github.com/stackbuilders/dotenv-hs) to load some of them from a file.
[envy](https://www.stackage.org/package/envy) is a really awesome way to read them.

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
