with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
let
in
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.lasspi;

  networking = {
    networkmanager = {
      enable = true;
    };
  };
  environment.systemPackages = with pkgs; [
    vim
    rxvt_unicode.terminfo
  ];
  services.openssh.enable = true;

  system.stateVersion = "21.05";
}
