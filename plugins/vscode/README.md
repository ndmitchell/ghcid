# VSCode haskell-ghcid

Integrates with [`ghcid`](https://github.com/ndmitchell/ghcid). To use:

* Get your project working so typing `ghcid` in the project root works. If you need to pass special flags to `ghcid` create a `.ghcid` file in the project root with the extra flags, e.g. `--command=cabal repl` or similar.
* Run the VS Code command (`Ctrl+Shift+P`) named "Start Ghcid".

These steps cause `ghcid` to be spawned as a fresh terminal, errors to appear in the VS Code Problems pane, red squiggles in the editor. Even though the errors are in the proper problems pane, I personally still prefer the output provided by the `ghcid` terminal, so still look at that.

## Other available commands

In addition to having the extension start `ghcid` itself, if you have to start `ghcid` in a special way, you can attach the extension to a running copy:

* Run `ghcid -o ghcid.txt` which will produce a file `ghcid.txt` which updates every time `ghcid` updates.
* Open `ghcid.txt` in VS Code as the active editor. Run the VS Code command (`Ctrl+Shift+P`) named "Watch Ghcid output".

## Features

Shows errors and warnings from Haskell in the Problems pane and inline in the document. Updates when files are saved.

## Requirements

Requires [`ghcid`](https://github.com/ndmitchell/ghcid) installed and on the `$PATH`.

## Local installation

Run:

    npm install
    npm install -g vsce
    rm haskell-ghcid-*.vsix
    vsce package
    code --install-extension haskell-ghcid-*.vsix
