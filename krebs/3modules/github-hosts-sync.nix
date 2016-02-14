{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  cfg = config.krebs.github-hosts-sync;

  out = {
    options.krebs.github-hosts-sync = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.github-hosts-sync";
    port = mkOption {
      type = types.int; # TODO port type
      default = 1028;
    };
    dataDir = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/var/lib/github-hosts-sync";
    };
    ssh-identity-file = mkOption {
      type = types.suffixed-str [".ssh.id_ed25519" ".ssh.id_rsa"];
      default = toString <secrets/github-hosts-sync.ssh.id_rsa>;
    };
  };

  imp = {
    systemd.services.github-hosts-sync = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        port = toString cfg.port;
      };
      serviceConfig = {
        PermissionsStartOnly = "true";
        SyslogIdentifier = "github-hosts-sync";
        User = user.name;
        Restart = "always";
        ExecStartPre = pkgs.writeScript "github-hosts-sync-init" ''
          #! /bin/sh
          set -euf
          install -m 0711 -o ${user.name} -d ${cfg.dataDir}
          install -m 0700 -o ${user.name} -d ${cfg.dataDir}/.ssh
          install -m 0400 -o ${user.name} \
            ${cfg.ssh-identity-file} \
            ${cfg.dataDir}/.ssh/${fileExtension cfg.ssh-identity-file}
        '';
        ExecStart = "${pkgs.github-hosts-sync}/bin/github-hosts-sync";
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
      home = cfg.dataDir;
    };
  };

  user = rec {
    name = "github-hosts-sync";
    uid = genid name;
  };

  # TODO move to lib?
  fileExtension = s: last (splitString "." s);

in out
