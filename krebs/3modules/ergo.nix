{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkOption types;
  inherit (pkgs) coreutils ergo;
  cfg = config.krebs.ergo;

  configFile = pkgs.writeText "ergo.conf" (builtins.toJSON cfg.config);
in

{

  ###### interface

  options = {

    krebs.ergo = {

      enable = mkEnableOption "Ergo IRC daemon";

      config = mkOption {
        type = (pkgs.formats.json {}).type;
        description = ''
          Ergo IRC daemon configuration file.
        '';
        default = {
          network = {
            name = "krebstest";
          };
          server = {
            name = "${config.networking.hostName}.r";
            listeners = {
              ":6667" = {};
            };
            casemapping = "permissive";
            enforce-utf = true;
            lookup-hostnames = false;
            ip-cloaking = {
              enabled = false;
            };
            forward-confirm-hostnames = false;
            check-ident = false;
            relaymsg = {
              enabled = false;
            };
            max-sendq = "1M";
            ip-limits = {
              count = false;
              throttle = false;
            };
          };
          datastore = {
            path = "/var/lib/ergo/ircd.db";
          };
          accounts = {
            authentication-enabled = true;
            registration = {
              enabled = true;
              email-verification = {
                enabled = false;
              };
            };
          };
          channels = {
            default-modes = "+nt";
          };
          limits = {
            nicklen = 32;
            identlen = 20;
            channellen = 64;
            awaylen = 390;
            kicklen = 390;
            topiclen = 390;
          };
        };
      };

    };

  };


  ###### implementation

  config = mkIf cfg.enable ({

    systemd.services.ergo = {
      description = "Ergo IRC daemon";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${ergo}/bin/ergo run --conf ${configFile}";
        DynamicUser = true;
        StateDirectory = "ergo";
      };
    };

  });
}
