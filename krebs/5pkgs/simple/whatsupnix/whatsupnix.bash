#!/usr/bin/env bash

# Prints build logs for failed derivations in quiet build mode (-Q).
# See https://github.com/NixOS/nix/issues/443
#
# Usage:
#
#    set -o pipefail
#    nix-build ... -Q ... | whatsupnix
#


GAWK=${GAWK:-gawk}
NIX_STORE=${NIX_STORE:-nix-store}

broken=$(mktemp)
trap 'rm -f -- "$broken"' EXIT

exec >&2

$GAWK -v broken="$broken" -f <(cat - <<- 'AWK'
  match($0, /builder for .*(\/nix\/store\/.+\.drv).* failed/, m) {
    print m[1] >> broken
  }
  { print $0 }
AWK
)

export NIX_PAGER='' # for nix-store
while read -r drv; do
  title="** FAILED $drv LOG  **"
  frame=${title//?/*}

  echo "$frame"
  echo "$title"
  echo "$frame"
  echo

  $NIX_STORE -l "$drv"

  echo
done < "$broken"

exit 0
