{ config, lib, pkgs, ...}:

{
  # generate private key with:
  # nix-store --generate-binary-cache-key my-secret-key my-public-key
  services.nix-serve = {
    enable = true;
    secretKeyFile = config.krebs.secret.files.nix-serve-key.path;
  };

  systemd.services.nix-serve = {
    requires = ["secret.service"];
    after = ["secret.service"];
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
  };
}

