with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/coreboot.nix>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/backups.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/steam.nix>
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
        libreoffice
        audacity
        zathura
        skype
        wine
      ];
      nixpkgs.config.firefox.enableAdobeFlash = true;
      services.xserver.enable = true;
      services.xserver.displayManager.lightdm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
      services.xserver.layout = "de";
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
          isNormalUser = true;
          useDefaultShell = true;
          createHome = true;
          extraGroups = [ "audio" ];
        };
      };
      security.sudo.extraConfig = ''
        bubsy ALL=(bitcoin) NOPASSWD: ALL
      '';
    }
    {
      #remote control
      environment.systemPackages = with pkgs; [
        x11vnc
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp -i retiolum --dport 5900"; target = "ACCEPT"; }
      ];
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
