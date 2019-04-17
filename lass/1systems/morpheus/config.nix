{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/power-action.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/steam.nix>
  ];

  krebs.build.host = config.krebs.hosts.morpheus;

  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  nixpkgs.config.packageOverrides = super: {
    steam = super.steam.override {
      withPrimus = true;
      extraPkgs = p: with p; [
        glxinfo
        nettools
        bumblebee
      ];
    };
  };


  services.xserver.desktopManager.default = "none";
  services.xserver.displayManager.lightdm.autoLogin = {
    enable = true;
    user = "lass";
    timeout = 5;
  };
}
