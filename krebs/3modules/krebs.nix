{ config, lib, ... }:
with lib;
let
  cfg = config.krebs;
in {
    options.krebs.enable = mkEnableOption "krebs";
    config = lib.mkIf config.krebs.enable {};
}
