{ coreutils, gnused, writeDashBin, zbar }:

writeDashBin "qrscan" ''
  set -efu

  tmpdir=$(${coreutils}/bin/mktemp --tmpdir -d qrscan.XXXXXXXX)
  codefile=$tmpdir/code

  cleanup() {
    ${coreutils}/bin/rm "$codefile"
    ${coreutils}/bin/rmdir "$tmpdir"
  }

  ${coreutils}/bin/mkfifo "$codefile"

  ${zbar}/bin/zbarcam > "$codefile" &
  zbarcampid=$!

  exec < "$codefile"
  while read -r code; do
    code=$(printf %s "$code" | ${gnused}/bin/sed -n 's/^QR-Code://p')
    if test -n "$code"; then
      ${coreutils}/bin/kill "$zbarcampid"
      echo "$code"
    fi
  done
''
