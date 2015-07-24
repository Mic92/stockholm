{ config, lib, ... }:

with lib;
let
  cfg = config.tv.retiolum;

  out = {
    imports = [ ../../3modules/krebs/retiolum.nix ];
    options.tv.retiolum = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.retiolum";

    connectTo = mkOption {
      type = with types; listOf str;
    };

    hosts = mkOption {
      type = types.path;
    };
  };

  imp = {
    krebs.retiolum = cfg;
  };

in out
