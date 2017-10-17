{ jq, writeDashBin }:

# usage: quote [ARGS...]
writeDashBin "quote" ''
  set -efu
  prefix=
  for x; do
    y=$(${jq}/bin/jq -nr --arg x "$x" '$x | @sh "\(.)"')
    echo -n "$prefix$y"
    prefix=' '
  done
  echo
''
