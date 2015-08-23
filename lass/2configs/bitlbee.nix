{ config, pkgs, ... }:

let
  lpkgs = import ../5pkgs { inherit pkgs; };
in {

  imports = [
    ../3modules/bitlbee.nix
  ];

  config.lass.bitlbee = {
    enable = true;
    bitlbeePkg = lpkgs.bitlbee;
  };
}
