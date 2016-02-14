{ config, pkgs, lib, ... }:
with lib;
let
  out = {
    options.krebs.lib = api;
  };
  api = mkOption {
    default = {};
    type = types.attrs;
  };
in out
