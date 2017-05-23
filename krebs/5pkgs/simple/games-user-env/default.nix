{ lib, pkgs, ... }: let

#TODO: make sure env exists prior to running
env_nix = pkgs.writeText "env.nix" ''
  { pkgs ? import <nixpkgs> {} }:

  (pkgs.buildFHSUserEnv {
    name = "simple-x11-env";
    targetPkgs = pkgs: with pkgs; [
      coreutils
    ];
    multiPkgs = pkgs: with pkgs; [
      alsaLib
      zlib
      xorg.libXxf86vm
      curl
      openal
      openssl_1_0_2
      xorg.libXext
      xorg.libX11
      xorg.libXrandr
      xorg.libXcursor
      xorg.libXinerama
      xorg.libXi
      mesa_glu
    ];
    runScript = "bash";
  }).env
'';


in pkgs.writeDashBin "games-user-env" ''
  nix-shell ${env_nix}
''
