#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

[[ -x '/usr/bin/fd' ]] && FD="fd" ||  FD="fdfind"

$FD . './typed-protocols' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
$FD . './typed-protocols-cborg' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
$FD . './typed-protocols-examples' -e hs -E Setup.hs -X stylish-haskell -c .stylish-haskell.yaml -i
