{ lib, pkgs, ... }:
{
  nixpkgs = lib.mkForce {
    derivation = let
      rev = (lib.importJSON ../../../krebs/nixpkgs.json).rev;
      sha256 = (lib.importJSON ../../../krebs/nixpkgs.json).sha256;
    in ''
      with import (builtins.fetchTarball {
        url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
        sha256 = "${sha256}";
      }) {};
      pkgs.fetchFromGitHub {
        owner = "nixos";
        repo = "nixpkgs";
        rev = "${rev}";
        sha256 = "${sha256}";
      }
    '';
  };
}
