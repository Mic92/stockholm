# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      <stockholm/jeschli>
      <stockholm/jeschli/2configs/urxvt.nix>
      <stockholm/jeschli/2configs/emacs.nix>
    ];

  krebs.build.host = config.krebs.hosts.bolide;
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdb"; # or "nodev" for efi only
  boot.initrd.luks.devices = [ {
    name = "bla";
    device = "/dev/disk/by-uuid/53f1eeaf-a7ac-456c-a2af-778dd8b8d5b0";
    preLVM = true;
    allowDiscards = true;
  } ];
#  networking.hostName = "bolide"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.shellAliases = {
    n = "nix-shell";
    stocki = pkgs.writeDash "deploy" ''
      cd ~/stockholm
      exec nix-shell -I stockholm="$PWD" --run 'deploy  --system="bolide"'
    '';
  };
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    wget vim
  # system helper
    ag
    curl
    copyq
    dmenu
    git
    i3lock
    keepass
    networkmanagerapplet
    rsync
    terminator
    tmux
    wget
  #  rxvt_unicode
  # editors
    emacs
  # internet
    thunderbird
    chromium
    google-chrome
  # programming languages
    go
    gcc
    ghc
    python35
    python35Packages.pip
  # go tools
    golint
    gotools
  # dev tools
    elmPackages.elm
    gnumake
    jetbrains.pycharm-professional
    jetbrains.webstorm
    jetbrains.goland
  # document viewer
    zathura
  ];

 # Some programs need SUID wrappers, can be configured further or are
 # started in user sessions.
 # programs.bash.enableCompletion = true;
 # programs.mtr.enable = true;
 # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

 # List services that you want to enable:

 # Enable the OpenSSH daemon.
 services.openssh.enable = true;

 # Open ports in the firewall.
 # networking.firewall.allowedTCPPorts = [ ... ];
 # networking.firewall.allowedUDPPorts = [ ... ];
 # Or disable the firewall altogether.
 # networking.firewall.enable = false;

 # Enable CUPS to print documents.
 # services.printing.enable = true;

 # Enable the X11 windowing system.
 services.xserver.enable = true;
 # services.xserver.layout = "us";
 # services.xserver.xkbOptions = "eurosign:e";

 services.xserver.displayManager.sddm.enable = true;
 services.xserver.windowManager.xmonad.enable = true;
 services.xserver.windowManager.xmonad.enableContribAndExtras = true;
 # Enable touchpad support.
 # services.xserver.libinput.enable = true;

 # Enable the KDE Desktop Environment.
 # services.xserver.displayManager.sddm.enable = true;
 # services.xserver.desktopManager.plasma5.enable = true;

 # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.jeschli = {
    isNormalUser = true;
    uid = 1000;
  };

 # This value determines the NixOS release with which your system is to be
 # compatible, in order to avoid breaking some software such as database
 # servers. You should change this only after NixOS release notes say you
 # should.
 system.stateVersion = "17.09"; # Did you read the comment?

}

