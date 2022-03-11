{ pkgs }:

pkgs.writeDashBin "ff" ''
  exec ${pkgs.firefoxWrapper}/bin/firefox "$@"
''
