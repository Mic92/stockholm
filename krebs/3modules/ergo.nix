{ config, lib, options, pkgs, ... }: {
  options = {
    krebs.ergo = {
      enable = lib.mkEnableOption "Ergo IRC daemon";
      config = lib.mkOption {
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
  config = let
    cfg = config.krebs.ergo;
    configFile = pkgs.writeJSON "ergo.conf" cfg.config;
  in lib.mkIf cfg.enable ({
    krebs.ergo.config =
      lib.mapAttrsRecursive (_: lib.mkDefault) options.krebs.ergo.config.default;
    systemd.services.ergo = {
      description = "Ergo IRC daemon";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.ergo}/bin/ergo run --conf ${configFile}";
        DynamicUser = true;
        StateDirectory = "ergo";
      };
    };
  });
}
