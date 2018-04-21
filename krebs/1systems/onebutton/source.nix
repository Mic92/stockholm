with import <stockholm/lib>;
let
  pkgs = import <nixpkgs> {};
  nixpkgs = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs-channels";
    rev = "6c064e6b"; # only binary cache for unstable arm6
    sha256 = "1rqzh475xn43phagrr30lb0fd292c1s8as53irihsnd5wcksnbyd";
  };
in import <stockholm/krebs/source.nix> {
  name = "onebutton";
  override.nixpkgs = mkForce {
    file = toString nixpkgs;
  };

}
