#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

fd . './typed-protocols' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
fd . './typed-protocols-cborg' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
fd . './typed-protocols-examples' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
