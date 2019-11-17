{ config, pkgs, lib, ... }:
with import <stockholm/lib>;
let

  nixpkgs_pre_node_10_17 = import (pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "81f4c491afbc8f0fe994ef946b1ac61cf1261577";
    sha256 = "0xvawrd9nq3ybvq2pdp5gyi8gygf0yimgp0bx1xggq6l8mvgrj71";
  }) {};
in {
  nixpkgs.config.packageOverrides = pkgs: {
    codimd = nixpkgs_pre_node_10_17.codimd;
  };

  services.nginx.virtualHosts.codimd = {
    enableACME = true;
    addSSL = true;
    serverName = "codi.lassul.us";
    locations."/".extraConfig = ''
      client_max_body_size 4G;
      proxy_set_header Host $host;
      proxy_pass http://localhost:3091;
    '';
  };

  services.codimd = {
    enable = true;
    configuration = {
      db = {
        dialect = "sqlite";
        storage = "/var/lib/codimd/db.codimd.sqlite";
        useCDN = false;
      };
      port = 3091;
    };
  };
}

