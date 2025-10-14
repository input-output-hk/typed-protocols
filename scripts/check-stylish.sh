#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

[[ -x '/usr/bin/fd' ]] && FD="fd" ||  FD="fdfind"

$FD . './typed-protocols' -e hs -E Setup.hs -E Core.hs -E Channel.hs -E QuickCheck -X stylish-haskell -c .stylish-haskell.yaml -i
