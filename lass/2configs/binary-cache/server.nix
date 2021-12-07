{ config, lib, pkgs, stockholm, ...}:

{
  # generate private key with:
  # nix-store --generate-binary-cache-key my-secret-key my-public-key
  services.nix-serve = {
    enable = true;
    secretKeyFile = toString <secrets> + "/nix-serve.key";
    port = 5005;
  };

  services.nginx = {
    enable = true;
    virtualHosts.nix-serve = {
      serverAliases = [ "cache.prism.r" ];
      locations."/".extraConfig = ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '';
      locations."= /nix-cache-info".extraConfig = ''
        alias ${pkgs.writeText "cache-info" ''
          StoreDir: /nix/store
          WantMassQuery: 1
          Priority: 42
        ''};
      '';
    };
    virtualHosts."cache.krebsco.de" = {
      forceSSL = true;
      serverAliases = [ "cache.lassul.us" ];
      enableACME = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '';
    };
  };
}

