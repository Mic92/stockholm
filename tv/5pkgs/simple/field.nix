{ gawk, writeDashBin }:

writeDashBin "field" ''
  set -u
  exec ${gawk}/bin/awk -v n="$1" '{print$n}'
''
