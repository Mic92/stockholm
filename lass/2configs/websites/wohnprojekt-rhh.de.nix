{ config, ... }:

{
  imports = [
    ../../3modules/static_nginx.nix
  ];

  lass.staticPage = {
    "wohnprojekt-rhh.de" = {};
  };
}

