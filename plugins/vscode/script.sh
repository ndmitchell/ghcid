#!/usr/bin/env bash

set -euo pipefail
set -x

cmd="${1:-}"

case "$cmd" in
  build)
    yarn install --immutable
    yarn tsc --noEmit
    yarn eslint .
    yarn prettier --check . || echo "Prettier check failed, run 'yarn prettier --write .' to fix"
    yarn tsdown && cp out/src/extension.cjs out/src/extension.js
    yarn tsx --enable-source-maps test/ghcid-client.test.ts -v
    node out/src/timeout.cjs 15 node --enable-source-maps out/test/runTest.cjs
    yarn vsce package
    ;;
  build-ci)
    yarn install --immutable
    yarn tsc --noEmit
    yarn eslint .
    yarn prettier --check . || echo "Prettier check failed, run 'yarn prettier --write .' to fix"
    yarn tsdown && cp out/src/extension.cjs out/src/extension.js
    yarn tsx --enable-source-maps test/ghcid-client.test.ts -v
    # Same as build, but skip the test where we spawn vs code since there's no display in CI
    # node out/src/timeout.cjs 15 node --enable-source-maps out/test/runTest.cjs
    yarn vsce package
    ;;
  test-client-server)
    yarn install --immutable
    yarn tsdown && cp out/src/extension.cjs out/src/extension.js
    node out/src/timeout.cjs 15 node --enable-source-maps out/test/ghcid-client.test.cjs
    ;;
  test-ext)
    yarn install --immutable
    yarn tsdown && cp out/src/extension.cjs out/src/extension.js
    node out/src/timeout.cjs 15 node --enable-source-maps out/test/runTest.cjs
    ;;
  *)
    echo "Usage: $0 build"
    exit 1
    ;;
esac
