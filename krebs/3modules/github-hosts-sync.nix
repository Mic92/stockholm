{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.github-hosts-sync;

  out = {
    options.krebs.github-hosts-sync = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.github-hosts-sync";
    dataDir = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/var/lib/github-hosts-sync";
    };
    srcDir = mkOption {
      type = types.str;
      default = "${config.krebs.tinc.retiolum.confDir}/hosts";
      defaultText = "\${config.krebs.tinc.retiolum.confDir}/hosts";
    };
    ssh-identity-file = mkOption {
      type = types.suffixed-str [".ssh.id_ed25519" ".ssh.id_rsa"];
      default = toString <secrets/github-hosts-sync.ssh.id_ed25519>;
      defaultText = "‹secrets/github-hosts-sync.ssh.id_ed25519›";
    };
    url = mkOption {
      type = types.str;
      default = "git@github.com:krebs/hosts.git";
    };
    workTree = mkOption {
      type = types.absolute-pathname;
      default = "${cfg.dataDir}/cache";
    };
  };

  imp = {
    systemd.services.github-hosts-sync = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        GITHUB_HOST_SYNC_USER_MAIL = user.mail;
        GITHUB_HOST_SYNC_USER_NAME = user.name;
        GITHUB_HOST_SYNC_SRCDIR = cfg.srcDir;
        GITHUB_HOST_SYNC_WORKTREE = cfg.workTree;
        GITHUB_HOST_SYNC_URL = cfg.url;
      };
      serviceConfig = {
        PermissionsStartOnly = "true";
        SyslogIdentifier = "github-hosts-sync";
        User = user.name;
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = pkgs.writeDash "github-hosts-sync-init" ''
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

    users.users.${user.name} = {
      inherit (user) uid;
      group = user.name;
      home = cfg.dataDir;
      isSystemUser = true;
    };
  };

  users.groups.${user.name} = {};

  user = rec {
    mail = "${name}@${config.krebs.build.host.name}";
    name = "github-hosts-sync";
    uid = genid_uint31 name;
  };

  # TODO move to lib?
  fileExtension = s: last (splitString "." s);

in out
