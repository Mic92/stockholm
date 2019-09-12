{ config, pkgs, lib, ... }:
let
  mainUser = config.krebs.build.user.name;
  unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in
{
  imports = [
    <stockholm/jeschli>
    ./hardware-configuration.nix
    <home-manager/nixos>
    <stockholm/jeschli/2configs/emacs.nix>
    <stockholm/jeschli/2configs/urxvt.nix>
    <stockholm/jeschli/2configs/steam.nix>
    <stockholm/jeschli/2configs/virtualbox.nix>
   ];
  krebs.build.host = config.krebs.hosts.brauerei;
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  # or "nodev" for efi only
  boot.initrd.luks.devices = [ {
    name = "root";
    device = "/dev/sda2";
    preLVM = true;
    allowDiscards = true;
  } ];
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Amsterdam";

  nixpkgs.config.allowUnfree = true;

  environment.shellAliases = {
    # emacs aliases
    ed = "emacsclient";
    edc = "emacsclient --create-frame";
    # nix aliases
    ns = "nix-shell";
    # krops
    deploy = pkgs.writeDash "deploy" ''
      set -eu
      export SYSTEM="$1"
      $(nix-build $HOME/stockholm/jeschli/krops.nix --no-out-link --argstr name "$SYSTEM" -A deploy)
    '';
 };

  environment.systemPackages = with pkgs; [
    # system helper
    acpi
    ag
    copyq
    curl
    dmenu
    aspell
    ispell
    rofi
    xdotool
    git
    gnupg
    i3lock
    keepass
    networkmanagerapplet
    pavucontrol
    rsync
    terminator
    tmux
    wget
  # editors
    emacs
  # internet
    chromium
    firefox
    google-chrome
    thunderbird
  # programming languages
    elixir
    elmPackages.elm
    exercism
    gcc9
    ccls
    unstable.clang_8
    ghc
    go
    python37
    python37Packages.pip
    pipenv
  # dev tools
    gnumake
    jetbrains.clion
    jetbrains.goland
    jetbrains.pycharm-professional
    jetbrains.webstorm
    vscode
  # document viewer
    evince
    zathura
  # go tools
    golint
    gotools
  # rust
    cargo
    rustracer
    rustup
  # orga tools
    taskwarrior
  # xorg
    xorg.xbacklight
  # tokei
    tokei
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

#  home-manager.useUserPackages = true;
#  home-manager.users.jeschli = {
#    home.stateVersion = "19.03";
#  };
#  home-manager.enable = true;

#  home-manager.users.jeschli.home.file = {
#     ".emacs.d" = {
#       source = pkgs.fetchFromGitHub {
#         owner = "jeschli";
#         repo = "emacs.d";
#         rev = "8ed6c40";
#         sha256 = "1q2y478srwp9f58l8cixnd2wj51909gp1z68k8pjlbjy2mrvibs0";
#       };
#       recursive = true;
#     };
#  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
#  services.emacs.enable = true;

  virtualisation.docker.enable = true;

  services.xserver = {
    enable = true;

    desktopManager = {
      xfce.enable = true;
      gnome3.enable = true;
    };

  };

  services.xserver.windowManager.i3.enable = true;

  users.extraUsers.jeschli = { # TODO: define as krebs.users
    isNormalUser = true;
    extraGroups = ["docker" "vboxusers" "audio"];
    uid = 1000;
  };
  users.extraUsers.blafoo = {
    isNormalUser = true;
    extraGroups = ["audio"];
    uid = 1002;
  };
  users.extraUsers.jamie = {
    isNormalUser = true;
    uid = 1001; # TODO genid
  };
  users.users.dev = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.lass.pubkey
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDB0d0JA20Vqn7I4lCte6Ne2EOmLZyMJyS9yIKJYXNLjbLwkQ4AYoQKantPBkTxR75M09E7d3j5heuWnCjWH45TrfQfe1EOSSC3ppCI6C6aIVlaNs+KhAYZS0m2Y8WkKn+TT5JLEa8yybYVN/RlZPOilpj/1QgjU6CQK+eJ1k/kK+QFXcwN82GDVh5kbTVcKUNp2tiyxFA+z9LY0xFDg/JHif2ROpjJVLQBJ+YPuOXZN5LDnVcuyLWKThjxy5srQ8iDjoxBg7dwLHjby5Mv41K4W61Gq6xM53gDEgfXk4cQhJnmx7jA/pUnsn2ZQDeww3hcc7vRf8soogXXz2KC9maiq0M/svaATsa9Ul4hrKnqPZP9Q8ScSEAUX+VI+x54iWrnW0p/yqBiRAzwsczdPzaQroUFTBxrq8R/n5TFdSHRMX7fYNOeVMjhfNca/gtfw9dYBVquCvuqUuFiRc0I7yK44rrMjjVQRcAbw6F8O7+04qWCmaJ8MPlmApwu2c05VMv9hiJo5p6PnzterRSLCqF6rIdhSnuOwrUIt1s/V+EEZXHCwSaNLaQJnYL0H9YjaIuGz4c8kVzxw4c0B6nl+hqW5y5/B2cuHiumnlRIDKOIzlv8ufhh21iN7QpIsPizahPezGoT1XqvzeXfH4qryo8O4yTN/PWoA+f7o9POU7L6hQ== lhebendanz@nixos"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEZgHR1ZPDBMUjGWar/QmI2GiUkZM8pAXRyBDh8j3hGlxlS+0lsBV6bTAI5F13iyzTC4pCuEuDO2OlFB0scwjcOATci8phd8jTjOIDodqDaeQZXbshyuUBfyiAV6q0Sc+cUDV3D6GhzigH3t8EiQmvXmUGm916yFotT12o0dm83SCOh1nAf9ZveC1Hz/eEUTvgWvIb58OdUR5F/S5OVBnIIJZ8tcp0BP9lyjjJCcANWkYJlwaVcNNb0UarCRhvRtptFj+e/EPqQxSCaS2QcxW4zBsQ6C81TFf7WrdH+pwtFg0owlWsxv547sRLLiPf2h2YuQgSoAaW24N0SHhUqvOXd+JyaYw7MAF8Qh3jHm2iJQRgXNuIN0msFi1alwAevilL2mnfAt2biQ9sS9g+CVvQCwX3mg09E4Y3UmFLzvsJafD9meKVrjnDCcXySeAfts59eFmwKtMQ0qrEWaclzUiA6Ay3uD1zma8x1XELGTf8nxnXCGl8s2i2APn7y1Tcwep69DlENWSaReF5zBLIkCtIUDd+8xBFTF3yu5CpyRrRMKGa0QX/MtsQl4SGJWadOTwpM8joIbrIVfKkTNB2McxAjvo0iaRoBDm409gi2Ycy+NSoUV/KAIUG7OysAQZ62hr+E/Kw1ocJCIVI+9vzKx/EnEIHkCSwhYKl5393W7CShVJjJUcKcZddqX2smSShXq8rXPzhIHk1dAVn5Ff/vGZT9z9R0QN3z6Oa9QN5t5TjTdUDToqHTudqOpDxPl2c2yXK9wV+aoHFoML9AmbzTT1U1mKU7GXSoFACiKNzhDzkovyJGpWRyvisX5t75IfuVqvGGI8n3u8OhPMdyyOHRylVaciDzBMZ00xnIHB+dJG9IeYaMm9bW1Li4Jo0CWnogo2+olfHPMLijBuu+bsa5Kp6kFkccJYR/xqcSq0lVXkpGm692JI4dnMGjchipXEGh1gXof9jXHemMMBwjpLFGty+D0r5KdA33m+mIqc9hi0ShquA9nA7E1IxDlgE0gQg+P5ZOeeIN7q54AQmT8iCCCRyne2Kw57XxaGgZoLfj7VjjaeRlzBUglmtyq8B7/c0J3y41vt9Hxhj4sKD+vufZu+M9E6E936KsJlIi+3U0PtopM/b8L4jcH1JYpPljapsys8wkJZ1ymHf6Kj/0FHyi1V+GvquiVrlFN+aHECIzNlCiSMO4MqfPUO1A+s9zkG2ZgPNNv+LoZqnokjbmKM4kdxexMxaL/Eo9Nd/bzdYiFYXlllEL7Uox+yV0N3loQ2juh4zn+ctCnwHi+V9X4l4rB8amW96WrXiJ/WqEK2UO8St8dcQWhCsUUm2OawSrbYYZw5HhJwz/Rhz2UsdSc56s5OUiQLJqpILYvCnqSLlF4iZdRSdDQNpKn+le3CeGUl5UUuvK2BpKGrbPKx0i/2ZSEMxNA5GnDMx/NyiNyDBcoPu/XOlNi8VWsEbCtoTQRamvqHjOmNcPrxCxds+TaF8c0wMR720yj5sWq8= jeschli@nixos"
      "ssh-rsa AAAAB4NzaC1yc2EAAAADAQABAAACAQC1x9+OtNfwv6LxblnLHeBElxoxLfaYUyvqMrBgnrlkaPjylPv711bvPslnt+YgdPsZQLCoQ2t5f0x0j7ZOMYE9eyRrnr67ITO+Od05u3eCypWOZulekkDL0ZDeYdvoZKOWnbKWnQVRfYuLOEL/g5/9E7MLtIdID8e98b/qHzs/+wmuuDR3zHCNic0BKixgET/EgFvLWezWxJ6D/TTv/5sDAfrC+RUN8ad14sxjKIkS3nkAlm8bhrCxQKaHLUcCJWiweW0gPWYSlp64VHS5lchvqCJlPYQdx0XbwolvlLYru0w74ljLbi3eL35GFFyHSeEjQ73EtVwo53uVKTy7SAORU7JNg6xL9H3ChOLOknN9oHs1K7t/maMsATle0HAFcTuaOhELUmHM8dCJh3nPVWIkzHQ4o3fyaogrpt7/V5j6R1/Ozn7P9n4OdqrjiaWqHlz/XHeYNNWte+a0EW+NubC83yS0Cu3uhZ36C3RET2vNM25CyYOBn4ccClAozayQIb6Cif0tCafMRPgkSlogQd8+SqNZpTnmtllIT3VnT5smgrufy6HETDkrHjApDrsqLtMCFY83RFwt4QLv/L93O7IsGifzmEfD9qD7YBSMNs8ihBIUXPk9doHXvYS506YroxWOxe/C0rzzbaogxQT6JMd1ozfXitRD9v7iBIFAT4Kzjw== christopher.kilian@dcso.de"
    ];
  };


  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEZgHR1ZPDBMUjGWar/QmI2GiUkZM8pAXRyBDh8j3hGlxlS+0lsBV6bTAI5F13iyzTC4pCuEuDO2OlFB0scwjcOATci8phd8jTjOIDodqDaeQZXbshyuUBfyiAV6q0Sc+cUDV3D6GhzigH3t8EiQmvXmUGm916yFotT12o0dm83SCOh1nAf9ZveC1Hz/eEUTvgWvIb58OdUR5F/S5OVBnIIJZ8tcp0BP9lyjjJCcANWkYJlwaVcNNb0UarCRhvRtptFj+e/EPqQxSCaS2QcxW4zBsQ6C81TFf7WrdH+pwtFg0owlWsxv547sRLLiPf2h2YuQgSoAaW24N0SHhUqvOXd+JyaYw7MAF8Qh3jHm2iJQRgXNuIN0msFi1alwAevilL2mnfAt2biQ9sS9g+CVvQCwX3mg09E4Y3UmFLzvsJafD9meKVrjnDCcXySeAfts59eFmwKtMQ0qrEWaclzUiA6Ay3uD1zma8x1XELGTf8nxnXCGl8s2i2APn7y1Tcwep69DlENWSaReF5zBLIkCtIUDd+8xBFTF3yu5CpyRrRMKGa0QX/MtsQl4SGJWadOTwpM8joIbrIVfKkTNB2McxAjvo0iaRoBDm409gi2Ycy+NSoUV/KAIUG7OysAQZ62hr+E/Kw1ocJCIVI+9vzKx/EnEIHkCSwhYKl5393W7CShVJjJUcKcZddqX2smSShXq8rXPzhIHk1dAVn5Ff/vGZT9z9R0QN3z6Oa9QN5t5TjTdUDToqHTudqOpDxPl2c2yXK9wV+aoHFoML9AmbzTT1U1mKU7GXSoFACiKNzhDzkovyJGpWRyvisX5t75IfuVqvGGI8n3u8OhPMdyyOHRylVaciDzBMZ00xnIHB+dJG9IeYaMm9bW1Li4Jo0CWnogo2+olfHPMLijBuu+bsa5Kp6kFkccJYR/xqcSq0lVXkpGm692JI4dnMGjchipXEGh1gXof9jXHemMMBwjpLFGty+D0r5KdA33m+mIqc9hi0ShquA9nA7E1IxDlgE0gQg+P5ZOeeIN7q54AQmT8iCCCRyne2Kw57XxaGgZoLfj7VjjaeRlzBUglmtyq8B7/c0J3y41vt9Hxhj4sKD+vufZu+M9E6E936KsJlIi+3U0PtopM/b8L4jcH1JYpPljapsys8wkJZ1ymHf6Kj/0FHyi1V+GvquiVrlFN+aHECIzNlCiSMO4MqfPUO1A+s9zkG2ZgPNNv+LoZqnokjbmKM4kdxexMxaL/Eo9Nd/bzdYiFYXlllEL7Uox+yV0N3loQ2juh4zn+ctCnwHi+V9X4l4rB8amW96WrXiJ/WqEK2UO8St8dcQWhCsUUm2OawSrbYYZw5HhJwz/Rhz2UsdSc56s5OUiQLJqpILYvCnqSLlF4iZdRSdDQNpKn+le3CeGUl5UUuvK2BpKGrbPKx0i/2ZSEMxNA5GnDMx/NyiNyDBcoPu/XOlNi8VWsEbCtoTQRamvqHjOmNcPrxCxds+TaF8c0wMR720yj5sWq8= jeschli@nixos"
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

}
