# VSCode haskell-ghcid

Integrates with Ghcid. Highly prototype. To use:

* Run `ghcid -o ghcid.txt` which will produce a file `ghcid.txt` which updates every time `ghcid` updates. Running the underlying `ghci` with `-ferror-spans` will significantly improve the errors reported.
* Open `ghcid.txt` in VS Code as the active editor. Run the VS Code command (`Ctrl+Shift+P`) named "Watch Ghcid output".

These steps cause the `ghcid` errors to appear in the VS Code Problems pane, and have red squiggles in the editor. Even though the errors are in the proper problems pane, I personally still prefer the output provided by the `ghcid` terminal, so still look at that.

## Features

Shows errors.

## Requirements

Requires ghcid and a lot of tedious manual setup (the steps above).

## Local installation

Run:

    npm install
    npm install -g vsce
    vsce package
    rm haskell-ghcid-*.vsix
    code --install-extension haskell-ghcid-*.vsix
