{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.lass.news;

  out = {
    options.lass.news = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "news";
    feeds = mkOption {
      type = types.listOf (types.submodule { options = {
        nick = mkOption {
          type = types.str;
        };
        feedurl = mkOption {
          type = types.str;
        };
        interval = mkOption {
          type = types.int;
          default = 1000;
        };
        channels = mkOption {
          type = types.listOf types.str;
        };
      };});
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "news";
        home = "/var/lib/news";
      };
    };
    ircServer = mkOption {
      type = types.str;
      default = "echelon.r";
      description = "to which server the bot should connect";
    };
  };

  imp = {

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
    };

    systemd.services = listToAttrs (map (feed:
      nameValuePair "news-${feed.nick}" {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          SyslogIdentifier = "news-${feed.nick}";
          User = cfg.user.name;
          PrivateTmp = true;
          Restart = "always";
          ExecStart = pkgs.writeDash "news-${feed.nick}" ''
            ${pkgs.haskellPackages.news}/bin/news '${feed.feedurl}' '${toString feed.interval}' \
              | ${pkgs.goify}/bin/goify \
              | while :; do
                ${pkgs.haskellPackages.kirk}/bin/ircout --nick '${feed.nick}' --host '${cfg.ircServer}' \
                  \${concatStringsSep " \\" feed.channels}
                done
          '';
        };
      }
    ) cfg.feeds);

  };

in out
