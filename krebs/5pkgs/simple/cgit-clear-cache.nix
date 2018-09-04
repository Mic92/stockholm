with import <stockholm/lib>;

{ cache-root ? "/tmp/cgit", findutils, writeDashBin }:

writeDashBin "cgit-clear-cache" ''
  set -efu
  ${findutils}/bin/find ${shell.escape cache-root} -type f -delete
''
