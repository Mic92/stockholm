{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    rustup
    gcc
  ];
}
