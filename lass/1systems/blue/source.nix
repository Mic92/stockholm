{ lib, pkgs, ... }:
{
  nixpkgs = lib.mkForce {
    file = toString (pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nixpkgs";
      rev = (lib.importJSON ../../../krebs/nixpkgs.json).rev;
      sha256 = (lib.importJSON ../../../krebs/nixpkgs.json).sha256;
    });
  };
}
