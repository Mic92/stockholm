{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let

  cfg = config.krebs.newsbot-js;

  enable = cfg != {};

  out = {
    options.krebs.newsbot-js = api;
    config = mkIf enable imp;
  };

  api = mkOption {
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        enable = mkEnableOption "Enable krebs newsbot" // { default = true; };

        channel = mkOption {
          type = types.str;
          default = "#${config._module.args.name}";
          description = "post the news in this channel";
        };
        feeds = mkOption {
          type = types.path;
          description = ''
            file with feeds to post
            format:
            $nick|$feedURI
          '';
        };
        ircServer = mkOption {
          type = types.str;
          default = "localhost";
          description = "to which server the bot should connect";
        };
        masterNick = mkOption {
          type = types.str;
          default = config._module.args.name;
          description = "nickname of the master bot";
        };
        package = mkOption {
          type = types.package;
          default = pkgs.newsbot-js;
          description = "newsbot package to use";
        };
        urlShortenerHost = mkOption {
          type = types.str;
          default = "go";
          description = "what server to use for url shortening, host";
        };
        urlShortenerPort = mkOption {
          type = types.str;
          default = "80";
          description = "what server to use for url shortening, port";
        };
      };
    }));
    default = {};
  };

  imp = {
    users.extraUsers.newsbot-js = {
      name = "newsbot-js";
      uid = genid "newsbot-js";
      description = "newsbot-js user";
      home = "/var/empty";
    };

    systemd.services = mapAttrs' (name: newsbot:
      nameValuePair "newsbot-${name}" {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        path = with pkgs; [
          newsbot-js
        ];

        environment = {
          irc_server = newsbot.ircServer;
          master_nick = newsbot.masterNick;
          news_channel = newsbot.channel;
          feeds_file = newsbot.feeds;
          url_shortener_host = newsbot.urlShortenerHost;
          url_shortener_port = newsbot.urlShortenerPort;
        };

        restartIfChanged = true;

        serviceConfig = {
          User = "newsbot-js";
          Restart = "always";
          ExecStart = "${newsbot.package}/bin/newsbot";
        };
      }
    ) cfg;
  };

in out
