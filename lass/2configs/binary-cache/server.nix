{ config, lib, pkgs, ...}:
let

  nix-serve-ng-src = builtins.fetchTarball {
    # Replace the URL and hash with whatever you actually need
    url    = "https://github.com/aristanetworks/nix-serve-ng/archive/1937593598bb1285b41804f25cd6f9ddd4d5f1cb.tar.gz";
    sha256 = "1lqd207gbx1wjbhky33d2r8xi6avfbx4v0kpsvn84zaanifdgz2g";
  };

  nix-serve-ng = import nix-serve-ng-src;

in
{
  imports = [ nix-serve-ng.nixosModules.default ];
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

