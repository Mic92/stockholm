{ pkgs, lib, ... }:

pkgs.writers.writeDashBin "irc-announce" ''
  set -euf

  IRC_SERVER=$1
  IRC_PORT=$2
  IRC_NICK=$3_$$
  IRC_CHANNEL=$4
  IRC_TLS=$5
  message=$6

  if test "$IRC_TLS" != 1; then
    unset IRC_TLS
  fi

  printf %s "$message" |
  ${pkgs.ircaids}/bin/ircsink \
      --nick="$IRC_NICK" \
      --port="$IRC_PORT" \
      --server="$IRC_SERVER" \
      --target="$IRC_CHANNEL" \
      ''${IRC_TLS:+--secure}
''
