{ config, lib, pkgs, ... }: let
  cfg = config.krebs.ssl;
in {
  options.krebs.ssl = {
    rootCA = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = builtins.readFile ../6assets/krebsRootCA.crt;
    };
    intermediateCA = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = builtins.readFile ../6assets/krebsAcmeCA.crt;
    };
    acmeURL = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "https://ca.r/acme/acme/directory";
    };
    trustRoot = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        whether to trust the krebs root CA.
        This implies that krebs can forge a certficate for every domain
      '';
    };
    trustIntermediate = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        whether to trust the krebs ACME CA.
        this only trusts the intermediate cert for .w and .r domains
      '';
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.trustRoot {
      security.pki.certificates = [ cfg.rootCA ];
    })
    (lib.mkIf cfg.trustIntermediate {
      security.pki.certificates = [ cfg.intermediateCA ];
    })
  ];
}
