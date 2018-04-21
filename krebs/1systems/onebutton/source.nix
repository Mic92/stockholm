let
  pkgs = import <nixpkgs> {};
in import <stockholm/krebs/source.nix> {
  name = "onebutton";
  nixpkgs.file = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs-channels";
    rev = "6c064e6b"; # only binary cache for unstable arm6
    sha256 = "0ssaaaaaaaaaaaawkgjk8c75mvhgn5z7g1dkb78r8vrih9428bb8";
  };
}
