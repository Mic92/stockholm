{ pkgs, lib, ... }:

let

  mpv = pkgs.symlinkJoin {
    name = "mpv";
    paths = [
      (pkgs.writeDashBin "mpv" ''
        exec ${pkgs.mpv}/bin/mpv --no-config "$@"
      '')
      pkgs.mpv
    ];
  };

in {
  environment.systemPackages = [
    mpv
  ];
}
