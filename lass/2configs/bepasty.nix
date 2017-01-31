{ config, pkgs, ... }:
with import <stockholm/lib>;

# secrets used:
#   wildcard.krebsco.de.crt
#   wildcard.krebsco.de.key
#   bepasty-secret.nix     <- contains single string

with import <stockholm/lib>;
let
  secKey = import <secrets/bepasty-secret.nix>;
  ext-dom = "paste.lassul.us" ;
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

      "${ext-dom}" = {
        nginx = {
          enableSSL = true;
          forceSSL = true;
          enableACME = true;
        };
        defaultPermissions = "read";
        secretKey = secKey;
      };
    };
  };
}
