{ config, lib, pkgs, ... }: with import <stockholm/lib>;
{
  environment.etc."binary-cache.pubkey".text =
    config.krebs.build.host.binary-cache.pubkey;

  nixpkgs.overlays = [
    (self: super: {
      nix-serve = self.haskellPackages.nix-serve-ng;
    })
  ];

  services.nix-serve = {
    enable = true;
    secretKeyFile = config.krebs.secret.files.binary-cache-seckey.path;
  };

  systemd.services.nix-serve = {
    after = [
      config.krebs.secret.files.binary-cache-seckey.service
    ];
    partOf = [
      config.krebs.secret.files.binary-cache-seckey.service
    ];
  };

  krebs.secret.files.binary-cache-seckey = {
    path = "/run/secret/nix-serve.key";
    owner.name = "nix-serve";
    source-path = toString <secrets> + "/nix-serve.key";
  };

  services.nginx = {
    enable = true;
    virtualHosts.nix-serve = {
      serverAliases = [
        "cache.${config.krebs.build.host.name}.hkw"
      ];
      locations."/".extraConfig = ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '';
    };
  };
}
