# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      <stockholm/jeschli>
      <stockholm/jeschli/2configs/virtualbox.nix>
      ./hardware-configuration.nix
      # ./dcso-vpn.nix
    ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  environment.shellAliases = {
    n = "nix-shell";
    gd = "cd /home/jeschli/go/src/gitlab.dcso.lolcat";
    gh = "cd /home/jeschli/go/src/github.com";
    stocki = pkgs.writeDash "deploy" ''
      cd ~/stockholm
      LOGNAME=jeschli exec nix-shell -I stockholm="$PWD" --run 'deploy  --system="bln"'
    '';
  };
  networking.hostName = lib.mkForce "BLN02NB0232"; # Define your hostname.
  networking.networkmanager.enable = true;
  # Set your time zone.
  time.timeZone = "Europe/Berlin";
  # Setup Packages
  nixpkgs.config.allowUnfree = true;
  environment.variables = { GOROOT= [ "${pkgs.go.out}/share/go" ]; };
  environment.systemPackages = with pkgs; [
  # system helper
    ag
    copyq
    dmenu
    git
    tig
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
  # databases
    sqlite
  # internet 
    thunderbird
    hipchat
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
    jetbrains.pycharm-professional
    jetbrains.webstorm
    jetbrains.goland
    jetbrains.datagrip
    texlive.combined.scheme-full
    pandoc
    redis
  # document viewer
    zathura
  ];


  programs.bash.enableCompletion = true;
  programs.vim.defaultEditor = true;
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
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.postscript-lexmark ];
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  # services.xserver.xrandrHeads = [
  #  { output = "eDP1"; }
  #  { output = "DP-2-2-8"; primary = true; }
  #  { output = "DP-2-1-8"; monitorConfig = ''Option "Rotate" "left"''; }
  # ];
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.displayManager.sddm.enable = true;
#  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.dpi = 100;
  fonts.fontconfig.dpi = 100;

#  services.xserver.displayManager.sessionCommands = ''
#    (sleep 1 && ${pkgs.xorg.xrandr}/bin/xrandr --output VIRTUAL1 --off --output eDP1 --mode 1920x1080 --pos 5120x688 --rotate normal --output DP1 --off --output DP2-1 --mode 2560x1440 --pos 2560x328 --rotate normal --output DP2-2 --primary --mode 2560x1440 --pos 0x328 --rotate normal --output DP2-3 --off --output HDMI2 --off --output HDMI1 --off --output DP2 --off
#'';

  users.extraUsers.jeschli = {
    isNormalUser = true;
    extraGroups = ["docker" "vboxusers"];
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

  # Gogland Debugger workaround
#  nixpkgs.config.packageOverrides = super: {
#    idea.gogland = lib.overrideDerivation super.idea.gogland (attrs: {
#      postFixup = ''
#	interp="$(cat $NIX_CC/nix-support/dynamic-linker)"
#	patchelf --set-interpreter $interp $out/gogland*/plugins/intellij-go-plugin/lib/dlv/linux/dlv
#        chmod +x $out/gogland*/plugins/intellij-go-plugin/lib/dlv/linux/dlv
#     '';
#    });
#  };

  virtualisation.docker.enable = true;
  

  # DCSO Certificates
  security.pki.certificateFiles = [
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC1G1.pem"; sha256 = "14vz9c0fk6li0a26vx0s5ha6y3yivnshx9pjlh9vmnpkbph5a7rh"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC2G1.pem"; sha256 = "0r1dd48a850cv7whk4g2maik550rd0vsrsl73r6x0ivzz7ap1xz5"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC3G1.pem"; sha256 = "0b5cdchdkvllnr0kz35d8jrmrf9cjw0kd98mmvzr0x6nkc8hwpdy"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC2G1.pem"; sha256 = "0rn57zv1ry9vj4p2248mxmafmqqmdhbrfx1plszrxsphshbk2hfz"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC3G1.pem"; sha256 = "0w88qaqhwxzvdkx40kzj2gka1yi85ipppjdkxah4mscwfhlryrnk"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC2G1.pem"; sha256 = "1z2qkyhgjvri13bvi06ynkb7mjmpcznmc9yw8chx1lnwc3cxa7kf"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC3G1.pem"; sha256 = "0smdjjvz95n652cb45yhzdb2lr83zg52najgbzf6lm3w71f8mv7f"; })
   # VBOX certs
   ./services.bundled.crt
  ]; 

  hardware.bluetooth.enable = true;
  krebs.build.host = config.krebs.hosts.bln;
}
