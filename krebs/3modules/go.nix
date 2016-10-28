{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.krebs.go;

  out = {
    options.krebs.go = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "Enable go url shortener";
    port = mkOption {
      type = types.str;
      default = "1337";
      description = "on which port go should run on";
    };
    redisKeyPrefix = mkOption {
      type = types.str;
      default = "go:";
      description = "change the Redis key prefix which defaults to `go:`";
    };
  };

  imp = {
    services.redis = {
      enable = mkDefault true;
      bind = mkDefault "127.0.0.1";
    };

    users.extraUsers.go = rec {
      name = "go";
      uid = genid name;
      description = "go url shortener user";
      home = "/var/lib/go";
      createHome = true;
    };

    systemd.services.go = {
      description = "go url shortener";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        go-shortener
      ];

      environment = {
        PORT = cfg.port;
        REDIS_KEY_PREFIX = cfg.redisKeyPrefix;
      };

      restartIfChanged = true;

      serviceConfig = {
        User = "go";
        Restart = "always";
        ExecStart = "${pkgs.go-shortener}/bin/go";
      };
    };
  };

in out
