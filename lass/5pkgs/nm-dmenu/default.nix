{ pkgs, ... }:

pkgs.writeDashBin "nm-dmenu" ''
  export PATH=$PATH:${pkgs.dmenu}/bin:${pkgs.networkmanagerapplet}/bin
  exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu "$@"
''
