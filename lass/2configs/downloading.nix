{ config, lib, pkgs, ... }:

with lib;

let
  rpc-password = import <secrets/transmission-pw.nix>;
in {
  imports = [
    ../3modules/folderPerms.nix
  ];

  users.extraUsers = {
    download = {
      name = "download";
      home = "/var/download";
      createHome = true;
      useDefaultShell = true;
      extraGroups = [
        "download"
      ];
      openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
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
      inherit rpc-password;
      peer-port = 51413;
    };
  };

  krebs.iptables = {
    enable = true;
    tables.filter.INPUT.rules = [
      { predicate = "-p tcp --dport 9091"; target = "ACCEPT"; }
      { predicate = "-p tcp --dport 51413"; target = "ACCEPT"; }
      { predicate = "-p udp --dport 51413"; target = "ACCEPT"; }
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
