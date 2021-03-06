#!/usr/bin/env bash
#
# Prints build logs for failed derivations in quiet build mode (-Q).
# See https://github.com/NixOS/nix/issues/443
#
# Usage:
#
#    nix-build ... -Q ... 2>&1 | whatsupnix [user@target[:port]]
#
# Exit Codes:
#
#   0     No failed derivations could be found.  This either means there where
#         no build errors, or stdin wasn't nix-build output.
#
#   1     Usage error; arguments couldn't be parsed.
#
#   2     Nix error; input looks like Nix failed.
#
#   3     Build error; at least one failed derivation could be found.
#

tmpdir=$(mktemp -d --tmpdir whatsupnix.XXXXXXXX)
failed_drvs=$tmpdir/failed_drvs; touch "$failed_drvs"
nix_errors=$tmpdir/nix_errors; touch "$nix_errors"
cleanup() {
  rm "$failed_drvs"
  rm "$nix_errors"
  rmdir "$tmpdir"
}
trap cleanup EXIT

exec >&2

gawk \
    -v failed_drvs="$failed_drvs" \
    -v nix_errors="$nix_errors" \
'
  /^(\033\[31;1m)?error:/ {
    print $0 >> nix_errors
  }
  match($0, /^builder for ‘(\/nix\/store\/[^’]+\.drv)’ failed/, m) {
    print m[1] >> failed_drvs
  }
  { print $0; fflush("/dev/stdout") }
'

case $# in
  0)
    print_log() {
      NIX_PAGER= nix-store -l "$1"
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
          env NIX_PAGER= nix-store -l "$1"
    }
    ;;
  *)
    echo "usage: whatsupnix [[USER@]HOST[:PORT]]" >&2
    exit 1
esac

while read -r drv; do
  title="** FAILED $drv LOG **"
  frame=${title//?/*}

  echo "$frame"
  echo "$title"
  echo "$frame"
  echo

  print_log "$drv"

  echo
done < "$failed_drvs"

if test -s "$nix_errors"; then
  exit 2
elif test -s "$failed_drvs"; then
  exit 3
else
  exit 0
fi
