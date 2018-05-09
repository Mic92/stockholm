{ pkgs, ... }:
{
  imports = [
    ../krebs
    ./2configs
  ];

  nixpkgs.config.packageOverrides = import ./5pkgs pkgs;
}
