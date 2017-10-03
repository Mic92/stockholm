{ config, pkgs, ... }:
with import <stockholm/lib>;

# secrets used:
#   wildcard.krebsco.de.crt
#   wildcard.krebsco.de.key
#   bepasty-secret.nix     <- contains single string

with import <stockholm/lib>;
let
  secKey = import <secrets/bepasty-secret.nix>;
  ext-doms = [
    "paste.lassul.us"
    "paste.krebsco.de"
  ];
in {

  services.nginx.enable = mkDefault true;
  krebs.bepasty = {
    enable = true;
    serveNginx= true;

    servers = {
      "paste.r" = {
        nginx = {
          serverAliases = [ "paste.retiolum" "paste.${config.krebs.build.host.name}" ];
        };
        defaultPermissions = "admin,list,create,read,delete";
        secretKey = secKey;
      };
    } //
    genAttrs ext-doms (ext-dom: {
      nginx = {
        forceSSL = true;
        enableACME = true;
      };
      defaultPermissions = "read,create";
      secretKey = secKey;
    });
  };
}
