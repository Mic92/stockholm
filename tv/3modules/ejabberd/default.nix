{ config, lib, pkgs, ... }@args: with import <stockholm/lib>; let

  cfg = config.tv.ejabberd;

  gen-dhparam = pkgs.writeDash "gen-dhparam" ''
    set -efu
    path=$1
    bits=2048
    # TODO regenerate dhfile after some time?
    if ! test -e "$path"; then
      ${pkgs.openssl}/bin/openssl dhparam "$bits" > "$path"
    fi
  '';

in {
  options.tv.ejabberd = {
    enable = mkEnableOption "tv.ejabberd";
    certfile = mkOption {
      type = types.absolute-pathname;
      default = toString <secrets> + "/ejabberd.pem";
    };
    hosts = mkOption {
      type = with types; listOf str;
    };
    pkgs.ejabberd = mkOption {
      type = types.package;
      default = pkgs.symlinkJoin {
        name = "ejabberd-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            exec ${pkgs.ejabberd}/bin/ejabberdctl \
                --config ${toFile "ejabberd.yaml" (import ./config.nix {
                  inherit pkgs;
                  config = cfg;
                })} \
                --logs ${shell.escape cfg.user.home} \
                --spool ${shell.escape cfg.user.home} \
                "$@"
          '')
          pkgs.ejabberd
        ];
      };
    };
    registration_watchers = mkOption {
      type = types.listOf types.str;
      default = [
        config.krebs.users.tv.mail
      ];
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "ejabberd";
        home = "/var/lib/ejabberd";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.symlinkJoin {
        name = "ejabberd-sudo-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            set -efu
            cd ${shell.escape cfg.user.home}
            exec /run/wrappers/bin/sudo \
                -u ${shell.escape cfg.user.name} \
                ${cfg.pkgs.ejabberd}/bin/ejabberdctl "$@"
          '')
          cfg.pkgs.ejabberd
        ];
      })
    ];

    krebs.systemd.services.ejabberd = {};

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = pkgs.writeDash "ejabberd" ''
          ${pkgs.coreutils}/bin/ln -s "$CREDENTIALS_DIRECTORY" /tmp/credentials
          ${gen-dhparam} /var/lib/ejabberd/dhfile
          exec ${cfg.pkgs.ejabberd}/bin/ejabberdctl foreground
        '';
        LoadCredential = [
          "certfile:${cfg.certfile}"
        ];
        PrivateTmp = true;
        SyslogIdentifier = "ejabberd";
        StateDirectory = "ejabberd";
        User = cfg.user.name;
        TimeoutStartSec = 60;
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
      group = cfg.user.name;
      isSystemUser = true;
    };

    users.groups.${cfg.user.name} = {};
  };
}
