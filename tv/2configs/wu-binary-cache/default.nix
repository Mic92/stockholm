{ config, lib, pkgs, ... }: with config.krebs.lib;
{
  services.nix-serve = assert config.krebs.build.host.name == "wu"; {
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
      server-names = [ "cache.wu.gg23" ];
      locations = singleton (nameValuePair "/" ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '');
    };
  };
}
