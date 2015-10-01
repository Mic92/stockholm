{ config, pkgs, ... }:

{
  imports = [
    ../3modules/folderPerms.nix
  ];

  users.extraUsers = {
    download = {
      name = "download";
      home = "/var/download";
      createHome = true;
      extraGroups = [
        "download"
      ];
    };

    transmission = {
      extraGroups = [
        "download"
      ];
    };
  };

  users.extraGroups = {
    download = {
      members = [
        "download"
        "transmission"
      ];
    };
  };

  services.transmission = {
    enable = true;
    settings = {
      download-dir = "/var/download/finished";
      incomplete-dir = "/var/download/incoming";
      incomplete-dir-enabled = true;

      rpc-authentication-required = true;
      rpc-whitelist-enabled = false;
      rpc-username = "download";
      #add rpc-password in secrets
      rpc-password = "test123";
    };
  };

  krebs.iptables = {
    enable = true;
    tables.filter.INPUT.rules = [
      { predicate = "-p tcp --dport 9091"; target = "ACCEPT"; }
    ];
  };

  lass.folderPerms = {
    enable = true;
    permissions = [
      {
        path = "/var/download";
        permission = "775";
        owner = "transmission:download";
      }
    ];
  };
}
