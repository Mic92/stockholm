#!/bin/sh
# usage: $0
set -eu
WD=$(dirname "$(realpath "$0")")
PUBKEYS=
for key in "$WD"/kmein/kmein.gpg "$WD"/lass/pgp/* "$WD"/makefu/pgp/* "$WD"/tv/pgp/*; do
  echo "$key" >&2
  keyid=$(gpg --with-colons --fingerprint --import-options show-only --import "$key" | grep fpr | cut -d : -f 10 | head -1)
  gpg --import "$key" >&2
  printf '5\ny\n' | gpg --command-fd 0 --expert --edit-key "$keyid" trust >&2
  PUBKEYS="${PUBKEYS}${keyid}\n"
done
printf "$PUBKEYS"
