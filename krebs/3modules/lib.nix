{ config, pkgs, lib, ... }:
with lib;
let
  out = {
    options.krebs.lib = api;
    config = imp;
  };
  api = mkOption {
    default = {};
    type = types.attrs;
  };
  imp = {
    krebs.lib = lib // import ../4lib { inherit lib; } // builtins;
  };
in out
