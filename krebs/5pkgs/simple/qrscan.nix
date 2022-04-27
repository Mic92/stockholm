{ pkgs }:

pkgs.writeDashBin "qrscan" ''
  set -efu

  ${pkgs.zbar}/bin/zbarcam -1 | ${pkgs.gnused}/bin/sed -n 's/^QR-Code://p'
''
