# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../.
      ../2configs/miefda.nix
      ../2configs/tlp.nix
      ../2configs/x220t.nix
      ../2configs/hardware-configuration.nix
      ../2configs/tinc-basic-retiolum.nix
      ../2configs/git.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
   i18n = {
  #   consoleFont = "Lat2-Terminus16";
     consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
   };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
 environment.systemPackages = with pkgs; [
     wget chromium 
   ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  #services.xserver.displayManager.kdm.enable = true;
   services.xserver.desktopManager = {
       xfce.enable = true;
       xterm.enable= false;
   }; 

  # Define a user account. Don't forget to set a password with ‘passwd’.
   users.extraUsers.miefda = {
     isNormalUser = true;
     initialPassword= "welcome";
     uid = 1000;
     extraGroups= [
        "wheel" 
     ];
   };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

  
  networking.hostName = config.krebs.build.host.name;

  krebs = {
    enable = true;
    search-domain = "retiolum";
    build = {
      host = config.krebs.hosts.bobby;
      user = config.krebs.users.miefda;
      source = {
        git.nixpkgs = {
          url = https://github.com/Lassulus/nixpkgs;
          rev = "363c8430f1efad8b03d5feae6b3a4f2fe7b29251";
          target-path = "/var/src/nixpkgs";
        };
        dir.secrets = {
          host = config.krebs.hosts.bobby;
          path = "/home/miefda/secrets/${config.krebs.build.host.name}";
        };
        dir.stockholm = {
          host = config.krebs.hosts.bobby;
          path = "/home/miefda/gits/stockholm";
        };
      };
    };
  };
}
