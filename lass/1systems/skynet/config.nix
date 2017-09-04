{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>

    <stockholm/lass/2configs/retiolum.nix>
    #<stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/backups.nix>
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

  #fileSystems = {
  #  "/bku" = {
  #    device = "/dev/mapper/pool-bku";
  #    fsType = "btrfs";
  #    options = ["defaults" "noatime" "ssd" "compress=lzo"];
  #  };
  #};

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="10:0b:a9:a6:44:04", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:d1:90:fc", NAME="et0"
  '';
}
