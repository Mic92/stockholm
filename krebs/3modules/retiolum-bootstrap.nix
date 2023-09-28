{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.krebs.retiolum-bootstrap;
in
{
  options.krebs.retiolum-bootstrap = {
    enable = mkEnableOption "retiolum boot strap for ${cfg.serverName}";
    serverName = mkOption {
        type = types.str;
        description = "hostname which serves tinc boot";
        default = "tinc.krebsco.de" ;
    };
    sslCertificate = mkOption {
        type = types.str;
        description = "Certificate file to use for ssl";
        default = "${config.krebs.secret.directory}/tinc.krebsco.de.crt" ;
    };
    sslCertificateKey = mkOption {
        type = types.str;
        description = "Certificate key to use for ssl";
        default = "${config.krebs.secret.directory}/tinc.krebsco.de.key";
    };
    # in use:
    #  ${config.krebs.secret.directory}/tinc.krebsco.de.crt
    #  ${config.krebs.secret.directory}/tinc.krebsco.de.key
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = mkDefault true;
      virtualHosts.retiolum-bootstrap = {
        inherit (cfg) serverName sslCertificate sslCertificateKey;
        forceSSL = true;
        extraConfig =''

          root ${pkgs.retiolum-bootstrap};
          try_files $uri $uri/retiolum.sh;
        '';
      };
    };
  };
}
