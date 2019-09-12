# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in
{
  imports =
    [
      ./hardware-configuration.nix
      <stockholm/jeschli>
      <stockholm/jeschli/2configs/urxvt.nix>
      <stockholm/jeschli/2configs/i3.nix>
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
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  networking.enableB43Firmware = true; #new

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
    rofi
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
    vscode
    go
    gcc9
    ccls
    unstable.clang_8
    ghc
    python37
    python37Packages.pip
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


 # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];

users.extraUsers.jeschli = {
    isNormalUser = true;
    extraGroups = ["docker" "vboxusers" "audio"];
    uid = 1000;
  };

  hardware.pulseaudio.enable = true;
 # This value determines the NixOS release with which your system is to be
 # compatible, in order to avoid breaking some software such as database
 # servers. You should change this only after NixOS release notes say you
 # should.
 system.stateVersion = "17.09"; # Did you read the comment?

}

