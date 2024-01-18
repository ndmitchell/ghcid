# VSCode haskell-ghcid

Shows errors and warnings from [`ghcid`](https://github.com/ndmitchell/ghcid) in the Problems pane and inline as red squiggles in the editor. Updates when files are saved.

## Usage

Simply run `ghcid -o ghcid.txt`! `-o` instructs `ghcid` to write its output to a file every time it recompiles your code. This extension will automatically find and watch that file for updates.

## Spawning `ghcid` in VS Code

Alternatively, you can tell VS Code to spawn `ghcid` in an embedded terminal:

* Get your project working so typing `ghcid` in the project root works. If you need to pass special flags to `ghcid`, create a `.ghcid` file in the project root with the extra flags, e.g. `--command=cabal repl` or similar.
* Run the VS Code command (`Ctrl+Shift+P`) named "Start Ghcid".

## Requirements

Requires [`ghcid`](https://github.com/ndmitchell/ghcid) to be installed and on your `$PATH`.

## Local installation

Run:
    npm update
    npm install
    npm install -g vsce
    rm haskell-ghcid-*.vsix # optional
    vsce package
    code --install-extension haskell-ghcid-*.vsix

## Making releases of this extension

* Create a personal token following [the instructions](https://code.visualstudio.com/api/working-with-extensions/publishing-extension#get-a-personal-access-token), which involves visiting [this page](https://ndmitchell.visualstudio.com/_usersSettings/tokens).
* Run `vsce publish -p <token>`.

## Authors

* [**@ndmitchell**](https://github.com/ndmitchell) Neil Mitchell
* [**@chrismwendt**](https://github.com/chrismwendt) Chris Wendt
