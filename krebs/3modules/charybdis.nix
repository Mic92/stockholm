{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption singleton types;
  inherit (pkgs) coreutils charybdis;
  cfg = config.krebs.charybdis;

  configFile = pkgs.writeText "charybdis.conf" ''
    ${cfg.config}
  '';
in

{

  ###### interface

  options = {

    krebs.charybdis = {

      enable = mkEnableOption "Charybdis IRC daemon";

      config = mkOption {
        type = types.str;
        description = ''
          Charybdis IRC daemon configuration file.
        '';
      };

      statedir = mkOption {
        type = types.str;
        default = "/var/lib/charybdis";
        description = ''
          Location of the state directory of charybdis.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "ircd";
        description = ''
          Charybdis IRC daemon user.
        '';
      };

      group = mkOption {
        type = types.str;
        default = "ircd";
        description = ''
          Charybdis IRC daemon group.
        '';
      };

      motd = mkOption {
        type = types.nullOr types.lines;
        default = null;
        description = ''
          Charybdis MOTD text.

          Charybdis will read its MOTD from /etc/charybdis/ircd.motd .
          If set, the value of this option will be written to this path.
        '';
      };

    };

  };


  ###### implementation

  config = mkIf cfg.enable (lib.mkMerge [
    {
      users.users.${cfg.user} = {
        description = "Charybdis IRC daemon user";
        uid = config.ids.uids.ircd;
        group = cfg.group;
      };

      users.groups.${cfg.group} = {
        name = cfg.group;
        gid = config.ids.gids.ircd;
      };

      systemd.services.charybdis = {
        description = "Charybdis IRC daemon";
        wantedBy = [ "multi-user.target" ];
        environment = {
          BANDB_DBPATH = "${cfg.statedir}/ban.db";
        };
        serviceConfig = {
          ExecStart   = "${charybdis}/bin/charybdis -foreground -logfile /dev/stdout -configfile ${configFile}";
          Group = cfg.group;
          User = cfg.user;
          PermissionsStartOnly = true; # preStart needs to run with root permissions
        };
        preStart = ''
          ${coreutils}/bin/mkdir -p ${cfg.statedir}
          ${coreutils}/bin/chown ${cfg.user}:${cfg.group} ${cfg.statedir}
        '';
      };

    }

    (mkIf (cfg.motd != null) {
      environment.etc."charybdis/ircd.motd".text = cfg.motd;
    })
  ]);
}
