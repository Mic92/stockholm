{ config, lib, pkgs, ... }:
with import <stockholm/lib>;
let

  cfg = config.lass.hass;

in {
  options.lass.hass = {
    config = mkOption {
      default = {};
      type =  with lib.types; let
          valueType = nullOr (oneOf [
            bool
            int
            float
            str
            (attrsOf valueType)
            (listOf valueType)
          ]) // {
            description = "Yaml value";
            emptyValue.value = {};
          };
        in valueType;
    };
    love = mkOption {
      default = {};
      type =  with lib.types; let
          valueType = nullOr (oneOf [
            bool
            int
            float
            str
            (attrsOf valueType)
            (listOf valueType)
          ]) // {
            description = "Yaml value";
            emptyValue.value = {};
          };
        in valueType;
    };
  };

  config =
    assert versionOlder version "20.09";
    mkIf (cfg.config != {})
     {
      services.home-assistant.config = cfg.config;
      # services.home-assistant.lovelaceConfig = cfg.love;
    };
}

