# ghcid [![Hackage version](https://img.shields.io/hackage/v/ghcid.svg?style=flat)](http://hackage.haskell.org/package/ghcid) [![Build Status](http://img.shields.io/travis/ndmitchell/ghcid.svg?style=flat)](https://travis-ci.org/ndmitchell/ghcid)

Either "GHCi as a daemon" or "GHC + a bit of an IDE". Unlike other Haskell development tools, `ghcid` is intended to be _incredibly simple_. In particular, it doesn't integrate with any editors, doesn't depend on GHC the library and doesn't start web servers.

**Using it**

Run `cabal update && cabal install ghcid` to install it as normal. Then run `ghcid --height=10 "--command=ghci Main.hs"`. The `height` is the number of lines you are going to resize your console window to (defaults to 8), and the `command` is how you start this project in `ghci`. Personally, I always create a `.ghci` file at the root of all my projects, which usually reads something like:

    :set -fwarn-unused-binds -fwarn-unused-imports
    :load Main

If you have that, then you can pass `--command=ghci` (or nothing, since that is the default).

After that, resize your console and make it so you can see it while working in your editor. On Windows the console will automatically sit on top of all other windows. On Linux, you probably want to use your window manager to make it topmost or use a [tiling window manager](http://xmonad.org/).

**What you get**

On every save you'll see a list of the errors and warnings in your project. It uses `ghci` under the hood, so even relatively large projects should update their status pretty quickly. As an example:

    Main.hs:23:10:
        Not in scope: `verbosit'
        Perhaps you meant `verbosity' (imported from System.Console.CmdArgs)
    Util.hs:18:1: Warning: Defined but not used: `foo'

Or, if everything is good, you see:

    All good

Please [report any bugs](https://github.com/ndmitchell/ghcid/issues) you find.
