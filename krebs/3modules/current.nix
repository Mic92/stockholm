{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.krebs.current;

  out = {
    options.krebs.current = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.current";
    host = mkOption {
      type = types.host;
    };
    user = mkOption {
      type = types.user;
    };
  };

  imp = {
  };

in out
