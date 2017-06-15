#!/usr/bin/env bash

# Prints build logs for failed derivations in quiet build mode (-Q).
# See https://github.com/NixOS/nix/issues/443
#
# Usage:
#
#    set -o pipefail
#    nix-build ... -Q ... | whatsupnix [user@target[:port]]
#


GAWK=${GAWK:-gawk}
NIX_STORE=${NIX_STORE:-nix-store}

broken=$(mktemp)
trap 'rm -f -- "$broken"' EXIT

exec >&2

$GAWK -v broken="$broken" '
  match($0, /^builder for ‘(\/nix\/store\/[^’]+\.drv)’ failed/, m) {
    print m[1] >> broken
  }
  { print $0 }
'

case $# in
  0)
    print_log() {
      $NIX_STORE -l "$1"
    }
    ;;
  1)
    remote_user=${1%%@*}
    if test "$remote_user" = "$1"; then
      remote_user=root
    else
      set -- "${1#$remote_user@}"
    fi
    remote_port=${1##*:}
    if test "$remote_port" = "$1"; then
      remote_port=22
    else
      set -- "${1%:$remote_port}"
    fi
    remote_host=$1
    print_log() {
      ssh "$remote_user@$remote_host" -p "$remote_port" \
          nix-store -l "$1"
    }
    ;;
  *)
    echo "usage: whatsupnix [[USER@]HOST[:PORT]]" >&2
    exit 1
esac

export NIX_PAGER='' # for nix-store
while read -r drv; do
  title="** FAILED $drv LOG **"
  frame=${title//?/*}

  echo "$frame"
  echo "$title"
  echo "$frame"
  echo

  print_log "$drv"

  echo
done < "$broken"

exit 0
