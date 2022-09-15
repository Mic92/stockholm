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
    secretKeyFile = toString <secrets> + "/nix-serve.key";
  };

  services.nginx = {
    enable = true;
    virtualHosts.nix-serve = {
      serverAliases = [
        "cache.${config.krebs.build.host.name}.hkw"
        "cache.${config.krebs.build.host.name}.r"
      ];
      locations."/".extraConfig = ''
        proxy_pass http://localhost:${toString config.services.nix-serve.port};
      '';
    };
  };
}
