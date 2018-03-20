# ghcid [![Hackage version](https://img.shields.io/hackage/v/ghcid.svg?label=Hackage)](https://hackage.haskell.org/package/ghcid) [![Stackage version](https://www.stackage.org/package/ghcid/badge/lts?label=Stackage)](https://www.stackage.org/package/ghcid) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/ghcid.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/ghcid) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/ghcid.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/ghcid)

Either "GHCi as a daemon" or "GHC + a bit of an IDE". To a first approximation, it opens `ghci` and runs `:reload` whenever your source code changes, formatting the output to fit a fixed height console. Unlike other Haskell development tools, `ghcid` is intended to be _incredibly simple_. In particular, it doesn't integrate with any editors, doesn't depend on GHC the library and doesn't start web servers.

_Acknowledgements:_ This project incorporates significant work from [JPMoresmau](https://github.com/JPMoresmau), who is listed as a co-author.

### Using it

Run `cabal update && cabal install ghcid` to install it as normal. Then run `ghcid --height=8 --topmost "--command=ghci Main.hs"`. The `height` is the number of lines you are going to resize your console window to (defaults to height of the console). The `topmost` is to make the window sit above all others, which only works on Windows. The `command` is how you start your project in `ghci`. If you omit `--command` then it will default to `stack ghci` if you have the `stack.yaml` file and `.stack-work` directory, default to `ghci` if you have a `.ghci` file in the current directory, and otherwise default to `cabal repl`.

Personally, I always create a `.ghci` file at the root of all my projects, which usually [reads something like](https://github.com/ndmitchell/ghcid/blob/master/.ghci):

    :set -fwarn-unused-binds -fwarn-unused-imports
    :set -isrc
    :load Main

After that, resize your console and make it so you can see it while working in your editor. On Windows the console will automatically sit on top of all other windows. On Linux, you probably want to use your window manager to make it topmost or use a [tiling window manager](http://xmonad.org/).

### What you get

On every save you'll see a list of the errors and warnings in your project. It uses `ghci` under the hood, so even relatively large projects should update their status pretty quickly. As an example:

    Main.hs:23:10:
        Not in scope: `verbosit'
        Perhaps you meant `verbosity' (imported from System.Console.CmdArgs)
    Util.hs:18:1: Warning: Defined but not used: `foo'

Or, if everything is good, you see:

    All good

Please [report any bugs](https://github.com/ndmitchell/ghcid/issues) you find.

### Ghcid Integration

There are a few plugins that integrate Ghcid into editors, notably:

* [VS Code](plugins/vscode/)
* [nvim](plugins/nvim/)
* [Emacs](plugins/emacs/)

And a blog article on using it:

* [Auto-reload threepenny-gui apps during development](https://binarin.ru/post/auto-reload-threepenny-gui/)

### FAQ

* _This isn't as good as full IDE._ I've gone for simplicity over features. It's a point in the design space, but not necessarily the best point in the design space for you. For "real" IDEs see [the Haskell wiki](http://www.haskell.org/haskellwiki/IDEs).
* _If I delete a file and put it back it gets stuck._ Yes, that's a [bug in GHCi](https://ghc.haskell.org/trac/ghc/ticket/9648). If you see GHCi getting confused just kill `ghcid` and start it again.
* _I want to run my tests when files change._ You can pass any `ghci` expression with the `--test` flag, e.g. `--test=:main`, which will be run whenever the code is warning free (or pass `--warnings` for when the code is merely error free).
* _I want to run arbitrary commands when arbitrary files change._ This project reloads `ghci` when files loaded by `ghci` change. If you want a more general mechanism something like [Steel Overseer](https://github.com/schell/steeloverseer) will probably work better.
* _I want syntax highlighting in the error messages._ One option is to use Neovim or Emacs and run the terminal in a buffer whose file type is set to Haskell.
* _I'm not seeing pattern matching warnings._ Ghcid automatically appends `-fno-code` to the command line, which makes the reload cycle about twice as fast. Unfortunately GHC 8.0 suffers from [bug 10600](https://ghc.haskell.org/trac/ghc/ticket/10600) which means `-fno-code` also disables pattern matching warnings. Until that GHC bug is fixed either accept no pattern match warnings or use `-c` to specify a command line to start `ghci` that doesn't include `-fno-code`.
