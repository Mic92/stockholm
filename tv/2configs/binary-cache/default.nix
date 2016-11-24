{ config, lib, pkgs, ... }: with import <stockholm/lib>;
{
  environment.etc."binary-cache.pubkey".text =
    config.krebs.build.host.binary-cache.pubkey;

  services.nix-serve = {
    enable = true;
    secretKeyFile = config.krebs.secret.files.binary-cache-seckey.path;
  };

  systemd.services.nix-serve = {
    requires = ["secret.service"];
    after = ["secret.service"];
  };

  krebs.secret.files.binary-cache-seckey = {
    path = "/run/secret/nix-serve.key";
    owner.name = "nix-serve";
    source-path = toString <secrets> + "/nix-serve.key";
  };

  krebs.nginx = {
    enable = true;
    servers.nix-serve = {
      server-names = [
        "cache.${config.krebs.build.host.name}.gg23"
      ];
      locations = singleton (nameValuePair "/" ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '');
    };
  };
}
