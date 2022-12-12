with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/nfs-dl.nix>
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
        isNormalUser = true;
      };
      networking.networkmanager.enable = true;
      networking.wireless.enable = mkForce false;
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
      programs.chromium = {
        enable = true;
        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        ];
      };
      environment.systemPackages = with pkgs; [
        ark
        pavucontrol
        #firefox
        chromium
        hexchat
        networkmanagerapplet
        libreoffice
        audacity
        zathura
        skypeforlinux
        wine
        geeqie
        vlc
        zsnes
      ];
      nixpkgs.config.firefox.enableAdobeFlash = true;
      services.xserver.enable = true;
      services.xserver.displayManager.lightdm.enable = true;
      services.xserver.desktopManager.plasma5.enable = true;
      services.tlp.enable = lib.mkForce false;
      services.xserver.layout = "de";
    }
    {
      users = {
        groups.plugdev = {};
        users = {
          bitcoin = {
            name = "bitcoin";
            description = "user for bitcoin stuff";
            home = "/home/bitcoin";
            isNormalUser = true;
            useDefaultShell = true;
            createHome = true;
            extraGroups = [
              "audio"
              "networkmanager"
              "plugdev"
            ];
            packages = let
              unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
            in [
              pkgs.electrum
              pkgs.electron-cash
              unstable.ledger-live-desktop
            ];
          };
        };
      };
      hardware.ledger.enable = true;
      security.sudo.extraConfig = ''
        bubsy ALL=(bitcoin) NOPASSWD: ALL
      '';
    }
    {
      #remote control
      environment.systemPackages = with pkgs; [
        x11vnc
        # torbrowser
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
}
