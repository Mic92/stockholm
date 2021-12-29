{ config, lib, options, pkgs, ... }: {
  options = {
    krebs.ergo = {
      enable = lib.mkEnableOption "Ergo IRC daemon";
      config = lib.mkOption {
        type = (pkgs.formats.json {}).type;
        description = ''
          Ergo IRC daemon configuration file.
          https://raw.githubusercontent.com/ergochat/ergo/master/default.yaml
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
            autoupgrade = true;
            path = "/var/lib/ergo/ircd.db";
          };
          accounts = {
            authentication-enabled = true;
            registration = {
              enabled = true;
              allow-before-connect = true;
              throttling = {
                enabled = true;
                duration = "10m";
                max-attempts = 30;
              };
              bcrypt-cost = 4;
              email-verification.enabled = false;
              multiclient = {
                enabled = true;
                allowed-by-default = true;
                always-on = "opt-in";
                auto-away = "opt-in";
              };
            };
          };
          channels = {
            default-modes = "+ntC";
            registration = {
              enabled = true;
            };
          };
          limits = {
            nicklen = 32;
            identlen = 20;
            channellen = 64;
            awaylen = 390;
            kicklen = 390;
            topiclen = 390;
          };
          history = {
            enabled = true;
            channel-length = 2048;
            client-length = 256;
            autoresize-window = "3d";
            autoreplay-on-join = 0;
            chathistory-maxmessages = 100;
            znc-maxmessages = 2048;
            restrictions = {
              expire-time = "1w";
              query-cutoff = "none";
              grace-period = "1h";
            };
            retention = {
              allow-individual-delete = false;
              enable-account-indexing = false;
            };
            tagmsg-storage = {
              default = false;
              whitelist = [
                "+draft/react"
                "+react"
              ];
            };
          };
        };
      };
    };
  };
  config = let
    cfg = config.krebs.ergo;
    configFile = pkgs.writeJSON "ergo.conf" cfg.config;
  in lib.mkIf cfg.enable ({
    environment.etc."ergo.yaml".source = configFile;
    krebs.ergo.config =
      lib.mapAttrsRecursive (_: lib.mkDefault) options.krebs.ergo.config.default;
    systemd.services.ergo = {
      description = "Ergo IRC daemon";
      wantedBy = [ "multi-user.target" ];
      reloadIfChanged = true;
      restartTriggers = [ configFile ];
      serviceConfig = {
        ExecStart = "${pkgs.ergo}/bin/ergo run --conf /etc/ergo.yaml";
        ExecReload = "${pkgs.util-linux}/bin/kill -HUP $MAINPID";
        DynamicUser = true;
        StateDirectory = "ergo";
      };
    };
  });
}
