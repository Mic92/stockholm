{ pkgs, lib, ... }:

pkgs.writers.writeBashBin "stable-interrogate" ''
  set -xefu
  set -o pipefail

  export PATH=${lib.makeBinPath [
    pkgs.coreutils
    pkgs.curl
    pkgs.jq
  ]}

  STABLE_URL=''${STABLE_URL:-http://stable-confusion.r}

  (if test -e "$1"; then
    cat "$1"
  elif [[ "$1" =~ ^https?: ]]; then
    curl -fSs "$1"
  else
    echo 'input not recognized' >&2
    exit 1
  fi) | base64 |
    jq -Rsrc '{
      image: .,
      model: "deepdanbooru", # clip is broken right now :(
    }' |
    curl -Ssf "$STABLE_URL/sdapi/v1/interrogate" \
      -X POST -H 'Content-Type: application/json' -d @- |
    jq -r '.caption'
''
