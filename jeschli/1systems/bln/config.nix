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

#  services.xserver.windowManager.xmonad.enable = true;
#  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
#  services.xserver.displayManager.sddm.enable = true;
#  services.xserver.dpi = 100;
#  fonts.fontconfig.dpi = 100;

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
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC1G1.pem"; sha256 = "006j61q2z44z6d92638iin6r46r4cj82ipwm37784h34i5x4mp0d"; })
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC2G1.pem"; sha256 = "1nkd1rjcn02q9xxjg7sw79lbwy08i7hb4v4pn98djknvcmplpz5m"; })
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC3G1.pem"; sha256 = "094m12npglnnv1nf1ijcv70p8l15l00id44qq7rwynhcgxi5539i"; })

    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC2G1.pem"; sha256 = "1anfncdf5xsp219kryncv21ra87flpzcjwcc85hzvlwbxhid3g4x"; })
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC3G1.pem"; sha256 = "035kkfizyl5dndj7rhvmy91rr75lakqbqgjx4dpiw0kqq369mz8r"; })
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC2G1.pem"; sha256 = "14fpzx1qjs9ws9sz0y7pb6j40336xlckkqcm2rc5j86yn7r22lp7"; })
    (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC3G1.pem"; sha256 = "1yjl3kyw4chc8vw7bnqac2h9vn8dxryw7lr7i03lqi9sdvs4108s"; })
  ];


  hardware.bluetooth.enable = true;
  krebs.build.host = config.krebs.hosts.bln;
}
