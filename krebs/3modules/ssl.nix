{ config, lib, pkgs, ... }: let
  cfg = config.krebs.ssl;
in {
  options.krebs.ssl = {
    rootCA = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = ''
        -----BEGIN CERTIFICATE-----
        MIIC0jCCAjugAwIBAgIJAKeARo6lDD0YMA0GCSqGSIb3DQEBBQUAMIGBMQswCQYD
        VQQGEwJaWjESMBAGA1UECAwJc3RhdGVsZXNzMRAwDgYDVQQKDAdLcmVic2NvMQsw
        CQYDVQQLDAJLTTEWMBQGA1UEAwwNS3JlYnMgUm9vdCBDQTEnMCUGCSqGSIb3DQEJ
        ARYYcm9vdC1jYUBzeW50YXgtZmVobGVyLmRlMB4XDTE0MDYxMTA4NTMwNloXDTM5
        MDIwMTA4NTMwNlowgYExCzAJBgNVBAYTAlpaMRIwEAYDVQQIDAlzdGF0ZWxlc3Mx
        EDAOBgNVBAoMB0tyZWJzY28xCzAJBgNVBAsMAktNMRYwFAYDVQQDDA1LcmVicyBS
        b290IENBMScwJQYJKoZIhvcNAQkBFhhyb290LWNhQHN5bnRheC1mZWhsZXIuZGUw
        gZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAMs/WNyeQziccllLqom7bfCjlh6/
        /qx9p6UOqpw96YOOT3sh/mNSBLyNxIUJbWsU7dN5hT7HkR7GwzpfKDtudd9qiZeU
        QNYQ+OL0HdOnApjdPqdspZfKxKTXyC1T1vJlaODsM1RBrjLK9RUcQZeNhgg3iM9B
        HptOCrMI2fjCdZuVAgMBAAGjUDBOMB0GA1UdDgQWBBSKeq01+rAwp7yAXwzlwZBo
        3EGVLzAfBgNVHSMEGDAWgBSKeq01+rAwp7yAXwzlwZBo3EGVLzAMBgNVHRMEBTAD
        AQH/MA0GCSqGSIb3DQEBBQUAA4GBAIWIffZuQ43ddY2/ZnjAxPCRpM3AjoKIwEj9
        GZuLJJ1sB9+/PAPmRrpmUniRkPLD4gtmolDVuoLDNAT9os7/v90yg5dOuga33Ese
        725musUbhEoQE1A1oVHrexBs2sQOplxHKsVXoYJp2/trQdqvaNaEKc3EeVnzFC63
        80WiO952
        -----END CERTIFICATE-----
      '';
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
