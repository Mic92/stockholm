{ config, lib, pkgs, ... }:
# bln config file
{
  imports = [
    ./hardware-configuration.nix
    <stockholm/jeschli>
    <stockholm/jeschli/2configs/virtualbox.nix>
    <stockholm/jeschli/2configs/urxvt.nix>
    <stockholm/jeschli/2configs/emacs.nix>
    <stockholm/jeschli/2configs/xdg.nix>
    <stockholm/jeschli/2configs/xserver>
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  jeschliFontSize = 20;

  environment.shellAliases = {
    n = "nix-shell";
    gd = "cd /home/markus/go/src/gitlab.dcso.lolcat";
    gh = "cd /home/markus/go/src/github.com";
    stocki = pkgs.writeDash "deploy" ''
      cd ~/stockholm
      LOGNAME=jeschli exec nix-shell -I stockholm="$PWD" --run 'deploy  --system="bln"'
    '';
  };
  networking.hostName = lib.mkForce "BLN02NB0232";
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Setup Packages
  nixpkgs.config.allowUnfree = true;
  environment.variables = { GOROOT= [ "${pkgs.go.out}/share/go" ]; };
  environment.systemPackages = with pkgs; [
    termite
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
    elmPackages.elm
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

  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.postscript-lexmark ];

  # Enable the X11 windowing system.
  services.xserver.videoDrivers = [ "nvidia" ];

  users.extraUsers.jeschli = {
    isNormalUser = true;
    extraGroups = ["docker" "vboxusers" "audio"];
    uid = 1000;
  };

  system.stateVersion = "17.09";
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
  ];

  hardware.bluetooth.enable = true;
  krebs.build.host = config.krebs.hosts.bln;
}
