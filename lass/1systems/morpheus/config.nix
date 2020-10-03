{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.morpheus;

  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";

  environment.systemPackages = with pkgs; [
    gitAndTools.hub
    nix-review
    firefox
    ag
  ];

  services.openssh.forwardX11 = true;
  programs.x2goserver.enable = true;
}
