# VSCode haskell-ghcid

Integrates with Ghcid. Highly prototype. To use:

* Run `ghcid -o ghcid.txt` which will produce a file `ghcid.txt` will updates every time the screen updates.
* Open `ghcid.txt` in VS Code and have it as the active editor. From the VS Code extension, run the command (`Ctrl+Shift+P`) named "Watch Ghcid output". 

Now your errors will be reflected in the problems pane.

## Features

Shows errors.

## Requirements

Requires ghcid and a lot of tedious manual setup (the steps above).

## Local installation

Run:

    npm install
    vsce package
    code --install-extension haskell-ghcid-0.0.1.vsix
