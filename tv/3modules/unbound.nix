{ config, lib, pkgs, ... }: {
  options.tv.unbound = {
    enable = lib.mkEnableOption "tv.unbound";
    DoH.enable = lib.mkEnableOption "tv.unbound.DoH";
    DoT.enable = lib.mkEnableOption "tv.unbound.DoT";
    host = lib.mkOption {
      type = lib.types.str;
    };
    useACMEHost = lib.mkOption {
      type = lib.types.str;
    };
  };
  imports = let
    cfg = config.tv.unbound;
  in [
    (lib.mkIf cfg.enable {
      services.unbound = {
        enable = true;
        settings.server = {
          access-control = [
            "::/0 allow"
            "0.0.0.0/0 allow"
          ];
          interface = [
            "127.0.0.1@53"
            "retiolum@53"
            "wiregrill@53"
          ];
          prefetch = true;
          prefetch-key = true;
        };
      };
      # Since we use this for local dns resolving, we don't want to stop/start
      # but just restart, so we quickly get it back.
      systemd.services.unbound.stopIfChanged = false;

      tv.iptables.input-retiolum-accept-udp = [ "domain" ];
      tv.iptables.input-wiregrill-accept-udp = [ "domain" ];
    })
    (lib.mkIf cfg.DoH.enable (let
      http-port = 8053;
      http-endpoint = "/query";
    in {
      services.unbound.package = pkgs.unbound-with-systemd.override {
        withDoH = true;
      };
      services.unbound.settings.server.interface = [
        "127.0.0.1@${toString http-port}"
      ];
      services.unbound.settings.server = {
        https-port = http-port;
        http-endpoint = http-endpoint;
        http-notls-downstream = true;
      };
      services.nginx.virtualHosts.${cfg.host} = {
        useACMEHost = cfg.useACMEHost;
        forceSSL = true;
        http2 = true;
        locations."/".return = ''404 "Not Found\n"'';
        locations.${http-endpoint}.extraConfig = ''
          grpc_pass grpc://127.0.0.1:${toString http-port};
        '';
      };

      tv.iptables.input-internet-accept-tcp = [ "https" ];
    }))
    (lib.mkIf cfg.DoT.enable {
      services.unbound.settings.server = {
        interface = [
          "::@853"
          "0.0.0.0@853"
        ];
        tls-service-key = "/run/credentials/unbound.service/tls-service-key";
        tls-service-pem = "/run/credentials/unbound.service/tls-service-pem";
      };
      krebs.systemd.services.unbound.restartIfCredentialsChange = true;
      systemd.services.unbound.serviceConfig.LoadCredential = [
        "tls-service-key:/var/lib/acme/${cfg.useACMEHost}/key.pem"
        "tls-service-pem:/var/lib/acme/${cfg.useACMEHost}/fullchain.pem"
      ];
      tv.iptables.input-internet-accept-tcp = [ "domain-s" ];
    })
  ];
}
