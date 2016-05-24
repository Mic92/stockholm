{ config, pkgs, lib, ... }:

let
  inherit (config.krebs.lib) genid;
  inherit (import ../../4lib { inherit lib pkgs; })
    ssl
    servePage;

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

