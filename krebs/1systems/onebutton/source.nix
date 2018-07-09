with import <stockholm/lib>;
let
  pkgs = import <nixpkgs> {};
  nixpkgs = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs-channels";
    rev = "nixos-unstable"; # only binary cache for unstable arm6
    sha256 = "1rqzh475xn43phagrr30lb0fd292c1s8as53irihsnd5wcksnbyd";
  };
in import <stockholm/krebs/source.nix> {
  name = "onebutton";
  override.nixpkgs = mkForce {
    file = toString nixpkgs;
  };

}
