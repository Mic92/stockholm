{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.matrix-synapse = {
    # synapse 1.60.0 errors during startup with:
    # https://github.com/matrix-org/synapse/issues/15809
    package = pkgs.matrix-synapse.overrideAttrs (oldAttrs: rec {
      version = "1.85.2";
      name = "matrix-synapse-${version}";
      src = pkgs.fetchFromGitHub {
        owner = "matrix-org";
        repo = "synapse";
        rev = "v${version}";
        hash = "sha256-pFafBsisBPfpDnFYWcimUuBgfFVPZzLna3yHeqIBAAE=";
      };
      cargoDeps = pkgs.rustPlatform.fetchCargoTarball {
        inherit src;
        name = "matrix-synapse-${version}";
        hash = "sha256-dnno+5Ma0YNYpmj3oZ5UG22uAanKwVT67BwQW+mHoFc=";
      };
      doCheck = false;
    });
    enable = true;
    settings = {
      server_name = "lassul.us";
      # registration_shared_secret = "yolo";
      database.name = "sqlite3";
      turn_uris  = [
        "turn:turn.matrix.org?transport=udp"
        "turn:turn.matrix.org?transport=tcp"
      ];
      listeners = [
        {
          port = 8008;
          bind_addresses = [ "::1" ];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = [ "client" ];
              compress = true;
            }
            {
              names = [ "federation" ];
              compress = false;
            }
          ];
        }
      ];
    };
  };
  services.nginx = {
    virtualHosts = {
      "lassul.us" = {
        locations."= /.well-known/matrix/server".extraConfig = ''
          add_header Content-Type application/json;
          return 200 '${builtins.toJSON {
            "m.server" = "matrix.lassul.us:443";
          }}';
        '';
        locations."= /.well-known/matrix/client".extraConfig = ''
          add_header Content-Type application/json;
          add_header Access-Control-Allow-Origin *;
          return 200 '${builtins.toJSON {
            "m.homeserver" = { "base_url" = "https://matrix.lassul.us"; };
            "m.identity_server" = { "base_url" = "https://vector.im"; };
          }}';
        '';
      };
      "matrix.lassul.us" = {
        forceSSL = true;
        enableACME = true;
        locations."/_matrix" = {
          proxyPass = "http://[::1]:8008";
        };
      };
    };
  };
}
