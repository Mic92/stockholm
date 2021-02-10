{ pkgs, lib, ... }: let
  unstable = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = (lib.importJSON ../../nixpkgs-unstable.json).rev;
    sha256 = (lib.importJSON ../../nixpkgs-unstable.json).sha256;
  };
in (import unstable {}).home-assistant
