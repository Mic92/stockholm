{ config, lib, pkgs, ... }:

with lib;

let
  createStaticPage = domain:
    {
      tv.nginx.servers."${domain}" = {
        server-names = [
          "${domain}"
          "www.${domain}"
        ];
        locations = [
          (nameValuePair "/" ''
            root /var/lib/http/${domain};
          '')
        ];
      };
      #networking.extraHosts = ''
      #  10.243.206.102 ${domain}
      #'';
    };

in {
  imports = [
    ../../3modules/tv/nginx.nix
    ../../3modules/lass/iptables.nix
  ] ++ map createStaticPage [
    "habsys.de"
    "pixelpocket.de"
    "karlaskop.de"
    "ubikmedia.de"
    "apanowicz.de"
    "aidsballs.de"
  ];

  lass.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
      ];
    };
  };


  tv.nginx = {
    enable = true;
    servers = {

      #"habsys.de" = {
      #  server-names = [
      #    "habsys.de"
      #    "www.habsys.de"
      #  ];
      #  locations = [
      #    (nameValuePair "/" ''
      #      root /var/lib/http/habsys.de;
      #    '')
      #  ];
      #};

      #"karlaskop.de" = {
      #  server-names = [
      #    "karlaskop.de"
      #    "www.karlaskop.de"
      #  ];
      #  locations = [
      #    (nameValuePair "/" ''
      #      root /var/lib/http/karlaskop.de;
      #    '')
      #  ];
      #};

      #"pixelpocket.de" = {
      #  server-names = [
      #    "pixelpocket.de"
      #    "www.karlaskop.de"
      #  ];
      #  locations = [
      #    (nameValuePair "/" ''
      #      root /var/lib/http/karlaskop.de;
      #    '')
      #  ];
      #};

    };
  };

  networking.extraHosts = ''
    10.243.206.102 habsys.de karlaskop.de pixelpocket.de ubikmedia.de apanowicz.de
    10.243.206.102 aidsballs.de
  '';

  #services.postgresql = {
  #  enable = true;
  #};

  #config.services.vsftpd = {
  #  enable = true;
  #  userlistEnable = true;
  #  userlistFile = pkgs.writeFile "vsftpd-userlist" ''
  #  '';
  #};
}
