#!/usr/bin/env bash

set -euo pipefail
export LC_ALL=C.UTF-8

[[ -x '/usr/bin/fd' ]] && FD="fd" ||  FD="fdfind"

$FD . --full-path typed-protocols -e hs --ignore-file ./scripts/check-stylish-ignore -X stylish-haskell -c .stylish-haskell.yaml -i
