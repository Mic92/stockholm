{ writeScriptBin }:

writeScriptBin "pssh" ''
  #! /bin/sh
  set -efu
  case ''${1-} in

  # TODO create plog with -o json | jq ... | map date

  # usage: pssh {-j,--journal} host...
  # Follow journal at each host.
  -j|--journal)
    shift
    "$0" journalctl -n0 -ocat --follow --all ::: "$@" \
      | while read line; do
          printf '%s %s\n' "$(date --rfc-3339=s)" "$line"
        done
    ;;

  -*)
    echo $0: unknown option: $1 >&2
    exit 1
    ;;

  # usage: pssh command [arg...] ::: host...
  # Run command at each host.
  *)
    exec parallel \
      --line-buffer \
      -j0 \
      --no-notice \
      --tagstring {} \
      ssh -T {} "$@"
    ;;

  esac
''
