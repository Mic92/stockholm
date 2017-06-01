{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.lass.pyload;

  out = {
    options.lass.pyload = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "pyload";
    user = mkOption {
      type = types.str;
      default = "download";
    };
  };

  imp = {

    krebs.per-user.${cfg.user}.packages = [
      pkgs.pyload
      pkgs.spidermonkey
      pkgs.tesseract
    ];

    krebs.iptables.tables.filter.INPUT.rules = [
       { predicate = "-p tcp --dport 9099"; target = "ACCEPT"; }
    ];
    systemd.services.pyload = {
      description = "pyload";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        pyload
        spidermonkey
        tesseract
        dnsmasq
      ];

      restartIfChanged = true;

      serviceConfig = {
        Restart = "always";
        ExecStart = "${pkgs.pyload}/bin/pyLoadCore";
        User = cfg.user;
      };
    };

  };

in out
