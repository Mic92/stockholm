{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.lass.go;

  out = {
    options.lass.go = api;
    config = mkIf cfg.enable imp;
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
    users.extraUsers.go = {
      name = "go";
      uid = 42774411; #genid go
      description = "go url shortener user";
      home = "/var/lib/go";
      createHome = true;
    };

    systemd.services.go = {
      description = "go url shortener";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        go
      ];

      environment = {
        PORT = cfg.port;
        REDIS_KEY_PREFIX = cfg.redisKeyPrefix;
      };

      restartIfChanged = true;

      serviceConfig = {
        User = "go";
        Restart = "always";
        ExecStart = "${pkgs.go}/bin/go";
      };
    };
  };

in out
