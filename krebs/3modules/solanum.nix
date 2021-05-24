{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption singleton types;
  inherit (pkgs) coreutils solanum;
  cfg = config.krebs.solanum;

  configFile = pkgs.writeText "solanum.conf" ''
    ${cfg.config}
  '';
in

{

  ###### interface

  options = {

    krebs.solanum = {

      enable = mkEnableOption "Solanum IRC daemon";

      config = mkOption {
        type = types.str;
        description = ''
          Solanum IRC daemon configuration file.
        '';
      };

      statedir = mkOption {
        type = types.path;
        default = "/var/lib/solanum";
        description = ''
          Location of the state directory of solanum.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "ircd";
        description = ''
          Solanum IRC daemon user.
        '';
      };

      group = mkOption {
        type = types.str;
        default = "ircd";
        description = ''
          Solanum IRC daemon group.
        '';
      };

      motd = mkOption {
        type = types.nullOr types.lines;
        default = null;
        description = ''
          Solanum MOTD text.

          Solanum will read its MOTD from /etc/solanum/ircd.motd .
          If set, the value of this option will be written to this path.
        '';
      };

    };

  };


  ###### implementation

  config = mkIf cfg.enable (lib.mkMerge [
    {
      users.users.${cfg.user} = {
        description = "Solanum IRC daemon user";
        uid = config.ids.uids.ircd;
        group = cfg.group;
      };

      users.groups.${cfg.group} = {
        gid = config.ids.gids.ircd;
      };

      systemd.tmpfiles.rules = [
        "d ${cfg.statedir} - ${cfg.user} ${cfg.group} - -"
      ];

      systemd.services.solanum = {
        description = "Solanum IRC daemon";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart   = "${solanum}/bin/solanum -foreground -logfile /dev/stdout -configfile ${configFile} -pidfile ${cfg.statedir}/ircd.pid";
          Group = cfg.group;
          User = cfg.user;
        };
      };

    }

    (mkIf (cfg.motd != null) {
      environment.etc."solanum/ircd.motd".text = cfg.motd;
    })
  ]);
}
