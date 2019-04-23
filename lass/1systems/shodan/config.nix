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
    <stockholm/lass/2configs/green-host.nix>
    <stockholm/lass/2configs/ssh-cryptsetup.nix>
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';
}
