{ lib, pkgs, ... }:

let
  krebs = import ../../krebs/4lib { inherit lib; };
in

with krebs;

krebs // rec {

  simpleScript = name: content:
    pkgs.stdenv.mkDerivation {
      inherit name;
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${pkgs.writeScript name content} $out/bin/${name}
      '';
    };
}
