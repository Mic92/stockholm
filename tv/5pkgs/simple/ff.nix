{ pkgs }:

pkgs.writeDashBin "ff" ''
  exec ${pkgs.firefox}/bin/firefox "$@"
''
