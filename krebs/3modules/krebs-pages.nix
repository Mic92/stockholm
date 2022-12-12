{ config, modulesPath, pkgs, ... }: let
  cfg = config.krebs.pages;
  lib = import ../../lib;
  extraTypes.nginx-vhost = lib.types.submodule (
    lib.recursiveUpdate
      (import (modulesPath + "/services/web-servers/nginx/vhost-options.nix")
              { inherit config lib; })
      {}
  );
in {
  options.krebs.pages = {
    enable = lib.mkEnableOption "krebs-pages";
    domain = lib.mkOption {
      type = lib.types.hostname;
      default = "krebsco.de";
    };
    nginx = lib.mkOption {
      type = extraTypes.nginx-vhost;
      default = {};
      example = lib.literalExpression /* nix */ ''
        {
          # To enable encryption and let let's encrypt take care of certificate
          enableACME = true;
          forceSSL = true;
        }
      '';
      description = lib.mkDoc ''
        With this option, you can customize the nginx virtualHost settings.
      '';
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.krebs-pages;
    };
  };
  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = lib.mkDefault true;
      virtualHosts.${cfg.domain} = lib.mkMerge [ cfg.nginx {
        root = lib.mkForce cfg.package;
      }];
    };
  };
}
