{ pkgs }:

pkgs.symlinkJoin {
  name = "flameshot-once-wrapper";
  paths = [
    (pkgs.writeDashBin "flameshot-once" ''
      export PATH=${pkgs.stdenv.lib.makeBinPath [
        pkgs.flameshot
        pkgs.qt5.qtbase
      ]}
      exec ${pkgs.haskellPackages.flameshot-once}/bin/flameshot-once "$@"
    '')
    pkgs.haskellPackages.flameshot-once
  ];
}
