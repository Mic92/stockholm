# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      <stockholm/jeschli>
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
 # boot.loader.grub.enable = true;
 # boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Define on which hard drive you want to install Grub.
#  boot.loader.grub.device = "/dev/disk/by-id/wwn-0x5002538844584d30"; # or "nodev" for efi only

  boot.initrd.luks.devices = [
    {
    name = "root";
    device = "/dev/disk/by-id/wwn-0x5002538844584d30-part2";
    preLVM = true;
    allowDiscards = true;
    }
  ];
#  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
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
  nixpkgs.config.allowUnfree = true;
  environment.shellAliases = { n = "nix-shell"; };
  environment.variables = { GOROOT= [ "${pkgs.go.out}/share/go" ]; };
  environment.systemPackages = with pkgs; [
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
    rxvt_unicode
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
    gnumake
  # document viewer
    zathura
   ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDMPuFzd6p3zZETIjoV5mRxCTQgeZk9s/P374mEDbj58wDTT0uGWu2JRf7cL1QRTvd5238tYl0eSHXH65+oaFB/mIvmiRnuw6qQODOMHlSbJN5/J2hEw/3v5gveiP1xNLfKlFhj6mmMRF7Etvzns/kLGLCSjj1UTlfo4iHmtinPmU+iQ8J4foS4cZj4oZesF8gndkc2EFMfL6en7EuU8GK6U9GtwKNL9N4UoUZXu8Nf00pkn/jrpmsDdI4zdVVAxWeu/Lo4li43EVixLcfwQiwzf6S9FvYIv30xPdy92GJSJwxm/QkYuc48VZWUoE+qThf3IEPETtX+MRZrM8RTtY01 markus@reaganzglas"
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
   services.xserver.enable = true;
   services.xserver.layout = "us";
   services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
   services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

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
  system.stateVersion = "18.03"; # Did you read the comment?

  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = ''
      export GOPATH=$HOME/go
      export PATH=$PATH:$GOPATH/bin
    '';
  };

  krebs.build.host = config.krebs.hosts.reagenzglas;

  hardware.bluetooth.enable = true;
}
