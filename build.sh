#!/usr/bin/env bash

set -euxo pipefail

hlint .
cabal test
