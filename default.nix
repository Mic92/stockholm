import <nixpkgs/nixos> {} // rec {
  lib = import ./lib;
  systems = with lib; let
    namespace = getEnv "LOGNAME";
    systemsDir = <stockholm> + "/${namespace}/1systems";
  in
    genAttrs
      (attrNames (filterAttrs (_: eq "directory") (readDir systemsDir)))
      (name: import <nixpkgs/nixos> {
        configuration = import (systemsDir + "/${name}/config.nix");
      });
}
