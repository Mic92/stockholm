{ config, ... }:

{
  imports = [
    ../../3modules/static_nginx.nix
  ];

  lass.staticPage = {
    "wohnprojekt-rhh.de" = {};
  };

  users.users.laura = {
    home = "/srv/http/wohnprojekt-rhh.de";
    createHome = true;
    useDefaultShell = true;
  };
}

