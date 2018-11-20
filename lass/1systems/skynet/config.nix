{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/blue-host.nix>
    {
      services.xserver.enable = true;
      services.xserver.desktopManager.xfce.enable = true;

      users.users.discordius = {
        uid = genid "diskordius";
        isNormalUser = true;
        extraGroups = [
          "audio"
          "networkmanager"
        ];
      };
      environment.systemPackages = with pkgs; [
        google-chrome
      ];
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.skynet;

  networking.wireless.enable = false;
  networking.networkmanager.enable = true;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';
}
