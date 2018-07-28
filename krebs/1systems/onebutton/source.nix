with import <stockholm/lib>;
let
  pkgs = import <nixpkgs> {};
  nixpkgs = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  };
in import <stockholm/krebs/source.nix> {
  name = "onebutton";
  override.nixpkgs = mkForce {
    file = toString nixpkgs;
  };

}
