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
    <stockholm/lass/2configs/nfs-dl.nix>
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  #media center
  users.users.media = {
    isNormalUser = true;
    uid = genid_uint31 "media";
    extraGroups = [ "video" "audio" ];
  };

  services.xserver.displayManager.lightdm.autoLogin = {
    enable = true;
    user = "media";
  };
}
