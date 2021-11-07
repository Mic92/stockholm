{ cache-root ? "/tmp/cgit", findutils, stockholm, writeDashBin }:

writeDashBin "cgit-clear-cache" ''
  set -efu
  ${findutils}/bin/find ${stockholm.lib.shell.escape cache-root} -type f -delete
''
