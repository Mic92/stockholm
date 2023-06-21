{ config, lib, pkgs, ... }: with import ../../lib/pure.nix { inherit lib; }; let
  cfg = config.krebs.exim;
in {
  options.krebs.exim = {
    enable = mkEnableOption "krebs.exim";
    config = mkOption {
      type = types.str;
      default = "";
      description = ''
        Verbatim Exim configuration.  This should not contain exim_user,
        exim_group, exim_path, or spool_directory.
      '';
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "exim";
        home = "/var/spool/exim";
      };
      description = ''
        User to use when no root privileges are required.
        In particular, this applies when receiving messages and when doing
        remote deliveries.  (Local deliveries run as various non-root users,
        typically as the owner of a local mailbox.) Specifying this value
        as root is not supported.
      '';
    };
    group = mkOption {
      type = types.group;
      default = {
        name = "exim";
      };
      description = ''
        Group to use when no root privileges are required.
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    environment = {
      etc."exim.conf".source = pkgs.writeEximConfig "exim.conf" /* exim */ ''
        exim_user = ${cfg.user.name}
        exim_group = ${cfg.group.name}
        exim_path = /run/wrappers/bin/exim
        spool_directory = ${cfg.user.home}

        # https://lists.exim.org/lurker/message/20171125.034842.d1d75cac.en.html
        chunking_advertise_hosts =

        ${cfg.config}
      '';
      systemPackages = [ pkgs.exim ];
    };
    krebs.setuid = {
      exim = {
        filename = "${pkgs.exim}/bin/exim";
        mode = "4111";
      };
      sendmail = {
        filename = "${pkgs.exim}/bin/exim";
        mode = "4111";
      };
    };
    systemd.services.exim = {
      restartTriggers = [
        config.environment.etc."exim.conf".source
      ];
      serviceConfig = {
        ExecStart = "+${pkgs.exim}/bin/exim -bdf -q30m";
        ExecReload = "+${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        User = cfg.user.name;
      };
      wantedBy = [ "multi-user.target" ];
    };
    users = {
      groups.${cfg.group.name} = {
        inherit (cfg.group) name gid;
      };
      users.${cfg.user.name} = {
        inherit (cfg.user) home name uid;
        createHome = true;
        group = cfg.group.name;
        isSystemUser = true;
      };
    };
  };
}
