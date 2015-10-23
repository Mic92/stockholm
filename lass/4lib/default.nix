{ lib, pkgs, ... }:

with lib;

{

  simpleScript = name: content:
    pkgs.stdenv.mkDerivation {
      inherit name;
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${pkgs.writeScript name content} $out/bin/${name}
      '';
    };

  getDefaultGateway = ip:
    concatStringsSep "." (take 3 (splitString "." ip) ++ ["1"]);

}
