{ pkgs }:

pkgs.symlinkJoin {
  name = "flameshot-once-wrapper";
  paths = [
    (pkgs.writeDashBin "flameshot-once" ''
      export PATH=${pkgs.stdenv.lib.makeBinPath [
        pkgs.flameshot
      ]}''${PATH:+:$PATH}
      exec ${pkgs.haskellPackages.flameshot-once}/bin/flameshot-once "$@"
    '')
    pkgs.haskellPackages.flameshot-once
  ];
}
