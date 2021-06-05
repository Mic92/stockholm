{ config, pkgs, ... }:

with builtins;
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    {
      # locke config
      i18n.defaultLocale ="de_DE.UTF-8";
      time.timeZone = "Europe/Berlin";
      services.xserver.enable = true;
      services.xserver.libinput.enable = false;
      users.users.locke = {
        uid = genid "locke";
        home = "/home/locke";
        group = "users";
        createHome = true;
        extraGroups = [
          "audio"
          "networkmanager"
        ];
        useDefaultShell = true;
        isNormalUser = true;
      };
      networking.networkmanager.enable = true;
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
      services.xserver.desktopManager.xfce = {
        enable = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.uriel;
  nixpkgs.config.allowUnfree = true;
}
