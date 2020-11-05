{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    <stockholm/lass/2configs/backup.nix>
    <stockholm/lass/2configs/blue-host.nix>
    <stockholm/lass/2configs/ssh-cryptsetup.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/gg23.nix>
    <stockholm/lass/2configs/hass>
    <stockholm/lass/2configs/br.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/home-media.nix>
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";

}
