# AGENTS.md

## Overview
- `ghcid` is a simple Haskell tool that runs `ghci`, reloads on file changes, and reports errors quickly.
- Main code lives in `src/` and `app/`. Tests live in `test/`. Editor integrations live in `plugins/`.

## Common Commands
- Build: `cabal build`
- Run tests: `cabal test`

## Working Notes
- Prefer structured concurrency (`bracket`, `withAsync`, `race`, the async package, etc.).
- Careful on Windows, some IO calls block asynchronous exceptions (e.g. `accept` on a socket).
