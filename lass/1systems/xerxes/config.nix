{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/mail.nix>
  ];

  krebs.build.host = config.krebs.hosts.xerxes;

  services.xserver = {
    displayManager.lightdm.autoLogin.enable = true;
    displayManager.lightdm.autoLogin.user = "lass";
  };

  boot.blacklistedKernelModules = [
    "xpad"
  ];

  lass.screenlock.enable = lib.mkForce false;
  krebs.syncthing = {
    folders = {
      the_playlist = {
        path = "/home/lass/tmp/the_playlist";
        peers = [ "mors" "phone" "prism" "xerxes" ];
      };
    };
  };
  krebs.permown = {
    "/home/lass/tmp/the_playlist" = {
      owner = "lass";
      group = "syncthing";
      umask = "0007";
    };
  };
}
