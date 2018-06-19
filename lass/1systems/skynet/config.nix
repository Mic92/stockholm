{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    #<stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    {
      # discordius config
      services.xserver.enable = true;
      users.users.discordius = {
        uid = genid "discordius";
        home = "/home/discordius";
        group = "users";
        createHome = true;
        extraGroups = [
          "audio"
          "networkmanager"
        ];
        useDefaultShell = true;
      };
      networking.networkmanager.enable = true;
      networking.wireless.enable = mkForce false;
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
      environment.systemPackages = with pkgs; [
        pavucontrol
        firefox
        hexchat
        networkmanagerapplet
      ];
      services.xserver.desktopManager.gnome3 = {
        enable = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.skynet;

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';
}
