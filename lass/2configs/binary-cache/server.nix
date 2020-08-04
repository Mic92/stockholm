{ config, lib, pkgs, ...}:

{
  # generate private key with:
  # nix-store --generate-binary-cache-key my-secret-key my-public-key
  services.nix-serve = {
    enable = true;
    secretKeyFile = config.krebs.secret.files.nix-serve-key.path;
  };

  systemd.services.nix-serve = {
    after = [
      config.krebs.secret.files.nix-serve-key.service
    ];
    requires = [
      config.krebs.secret.files.nix-serve-key.service
    ];
  };
  krebs.secret.files.nix-serve-key = {
    path = "/run/secret/nix-serve.key";
    owner.name = "nix-serve";
    source-path = toString <secrets> + "/nix-serve.key";
  };
  services.nginx = {
    enable = true;
    virtualHosts.nix-serve = {
      serverAliases = [ "cache.prism.r" ];
      locations."/".extraConfig = ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
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

