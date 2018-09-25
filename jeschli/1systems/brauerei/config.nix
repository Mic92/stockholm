# Edit this configuration file to define what should be installed on # your system.  Help is available in the configuration.nix(5) man page # and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, lib, ... }:
{
  imports = [
    <stockholm/jeschli>
    ./hardware-configuration.nix
    <stockholm/jeschli/2configs/urxvt.nix>
    <stockholm/jeschli/2configs/emacs.nix>
    <stockholm/jeschli/2configs/xdg.nix>
    <stockholm/jeschli/2configs/xserver>
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
    n = "nix-shell";
    stocki = pkgs.writeDash "deploy" ''
      cd ~/stockholm
      exec nix-shell -I stockholm="$PWD" --run 'deploy  --system="brauerei"'
    '';
    deploy = pkgs.writeDash "deploy" ''
      set -eu
      export SYSTEM="$1"
      $(nix-build $HOME/stockholm/jeschli/krops.nix --no-out-link --argstr name "$SYSTEM" -A deploy)
    '';
  };

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
  # editors
    emacs
  # internet
    thunderbird
    chromium
    google-chrome
  # programming languages
    elixir
    elmPackages.elm
    exercism
    go
    gcc
    ghc
    python35
    python35Packages.pip
    (vagrant.override {
      bundlerEnv = bundlerEnv.override {
        bundler = bundler.overrideAttrs (old: {
          name = "bundler-1.16.1";
          src = fetchurl {
            url = "https://rubygems.org/gems/bundler-1.16.1.gem";
            sha256 = "1s2nq4qnffxg3kwrk7cnwxcvfihlhxm9absl2l6d3qckf3sy1f22";
          };
        });
      };
    })
  # go tools
    golint
    gotools
  # rust
    cargo
    rustc
  # dev tools
    gnumake
    jetbrains.pycharm-professional
    jetbrains.webstorm
    jetbrains.goland
  # document viewer
    evince
    zathura
  # xorg
    xorg.xbacklight
    taskwarrior
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver = {

    # Don't install feh into systemPackages
    # refs <nixpkgs/nixos/modules/services/x11/desktop-managers>
    desktopManager.session = lib.mkForce [];

    enable = true;
    display = 11;
    tty = 11;

    dpi = 144;

#    videoDrivers = [ "nvidia" ];
    synaptics = {
      enable = true;
      twoFingerScroll = true;
      accelFactor = "0.035";
    };
  };

  users.extraUsers.jeschli = { # TODO: define as krebs.users
    isNormalUser = true;
    extraGroups = ["docker" "vboxusers" "audio"];
    uid = 1000;
  };
  users.extraUsers.jamie = {
    isNormalUser = true;
    uid = 1001; # TODO genid
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
