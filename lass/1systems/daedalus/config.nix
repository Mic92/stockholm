with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/coreboot.nix>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/backups.nix>
    {
      # bubsy config
      users.users.bubsy = {
        uid = genid "bubsy";
        home = "/home/bubsy";
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
      services.xserver.enable = true;
      services.xserver.displayManager.lightdm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
    }
    {
      krebs.per-user.bitcoin.packages = [
        pkgs.electrum
      ];
      users.extraUsers = {
        bitcoin = {
          name = "bitcoin";
          description = "user for bitcoin stuff";
          home = "/home/bitcoin";
          useDefaultShell = true;
          createHome = true;
        };
      };
      security.sudo.extraConfig = ''
        bubsy ALL=(bitcoin) NOPASSWD: ALL
      '';
    }
  ];

  time.timeZone = "Europe/Berlin";

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  krebs.build.host = config.krebs.hosts.daedalus;

  fileSystems = {
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="08:11:96:0a:5d:6c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:71:cb:35", NAME="et0"
  '';
}
