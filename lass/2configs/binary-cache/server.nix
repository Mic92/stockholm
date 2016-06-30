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
  krebs.nginx = {
    enable = true;
    servers.nix-serve = {
      server-names = [ "cache.prism.r" ];
      locations = lib.singleton (lib.nameValuePair "/" ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '');
    };
  };
}

