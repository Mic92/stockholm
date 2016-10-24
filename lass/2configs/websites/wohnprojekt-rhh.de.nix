{ config, pkgs, lib, ... }:

let
  inherit (import <stockholm/lib>)
    genid
  ;
  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    ssl
    servePage
  ;
in {
  imports = [
    ( ssl [ "wohnprojekt-rhh.de" ])
    ( servePage [ "wohnprojekt-rhh.de" ])
  ];

  users.users.laura = {
    home = "/srv/http/wohnprojekt-rhh.de";
    createHome = true;
    useDefaultShell = true;
  };
}

