{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  cfg = config.tv.github-hosts-sync;

  out = {
    options.tv.github-hosts-sync = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.github-hosts-sync";
    port = mkOption {
      type = types.int; # TODO port type
      default = 1028;
    };
    dataDir = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/var/lib/github-hosts-sync";
    };
    ssh-identity-file = mkOption {
      type = types.str; # TODO must be named *.ssh.{id_rsa,id_ed25519}
      default = "/root/src/secrets/github-hosts-sync.ssh.id_rsa";
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

          ssh_identity_file_target=$(
            case ${cfg.ssh-identity-file} in
              *.ssh.id_rsa|*.ssh.id_ed25519) echo ${cfg.dataDir}/.ssh/id_rsa;;
              *.ssh.id_ed25519) echo ${cfg.dataDir}/.ssh/id_ed25519;;
              *)
                echo "bad identity file name: ${cfg.ssh-identity-file}" >&2
                exit 1
            esac
          )

          mkdir -p ${cfg.dataDir}
          chown ${user.name}: ${cfg.dataDir}

          install \
            -o ${user.name} \
            -m 0400 \
            ${cfg.ssh-identity-file} \
            "$ssh_identity_file_target"

          ln -snf ${Zpkgs.github-known_hosts} ${cfg.dataDir}/.ssh/known_hosts
        '';
        ExecStart = "${Zpkgs.github-hosts-sync}/bin/github-hosts-sync";
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
      home = cfg.dataDir;
    };
  };

  user = {
    name = "github-hosts-sync";
    uid = 3220554646; # genid github-hosts-sync
  };

  Zpkgs = import ../../Zpkgs/tv { inherit pkgs; };
in
out
