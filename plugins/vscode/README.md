# VSCode haskell-ghcid

Integrates with Ghcid. Highly prototype. To use:

* Run `ghcid -o ghcid.txt` which will produce a file `ghcid.txt` will updates every time the screen updates.
* From the VS Code extension, run the command (`Ctrl+Shift+P`) named "Watch Ghcid output". You must have the `ghcid.txt` file open in VS Code when you run this command.

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
