with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/backup.nix>
    <stockholm/lass/2configs/steam.nix>
    {
      users.users.blacky = {
        uid = genid "blacky";
        home = "/home/blacky";
        group = "users";
        createHome = true;
        extraGroups = [
          "audio"
          "networkmanager"
          "video"
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
        chromium
        hexchat
        networkmanagerapplet
        vlc
      ];
      services.xserver.enable = true;
      services.xserver.displayManager.lightdm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
      services.xserver.layout = "de";
      users.mutableUsers = mkForce true;
      services.xserver.synaptics.enable = true;
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

  krebs.build.host = config.krebs.hosts.littleT;
}
