{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;
in {
  imports = [
    ./mpv.nix
    ./power-action.nix
    ./copyq.nix
    ./urxvt.nix
    ./xdg-open.nix
    ./yubikey.nix
    ./pipewire.nix
    ./tmux.nix
    ./xmonad.nix
    {
      krebs.per-user.lass.packages = [
        pkgs.sshuttle
      ];
      security.sudo.extraConfig = ''
        lass ALL= (root) NOPASSWD:SETENV: ${pkgs.sshuttle}/bin/.sshuttle-wrapped
      '';
    }
    { #font magic
      options.lass.fonts = {
        regular = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
        bold = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
        italic = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
      };
      config.krebs.xresources.resources.X = ''
        *.font:       ${config.lass.fonts.regular}
        *.boldFont:   ${config.lass.fonts.bold}
        *.italicFont: ${config.lass.fonts.italic}
      '';
    }
  ];

  users.users.mainUser.extraGroups = [ "audio" "video" ];

  time.timeZone = "Europe/Berlin";

  programs.ssh.agentTimeout = "10m";
  programs.ssh.startAgent = false;
  services.openssh.forwardX11 = true;

  environment.systemPackages = with pkgs; [
    acpi
    acpilight
    ripgrep
    cabal2nix
    dic
    dmenu
    font-size
    fzfmenu
    gimp
    gitAndTools.hub
    git-crypt
    git-preview
    dconf
    iodine
    libarchive
    lm_sensors
    ncdu
    nix-index
    nixpkgs-review
    nmap
    pavucontrol
    ponymix
    powertop
    rxvt_unicode-with-plugins
    sxiv
    taskwarrior
    termite
    transgui
    wirelesstools
    xclip
    xephyrify
    xorg.xhost
    xsel
    zathura
    flameshot-once
    (pkgs.writeDashBin "screenshot" ''
      set -efu

      ${pkgs.flameshot-once}/bin/flameshot-once
      ${pkgs.klem}/bin/klem
    '')
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      hack-font
      xorg.fontschumachermisc
      terminus_font_ttf
      inconsolata
    ];
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="backlight", ACTION=="add", \
    RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", \
    RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  services.xserver = {
    enable = true;
    layout = "us";
    display = mkForce 0;
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:escape";
    libinput.enable = true;
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+xmonad";
      sessionCommands = ''
        ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
      '';
    };
  };

  nixpkgs.config.packageOverrides = super: {
    dmenu = pkgs.writeDashBin "dmenu" ''
      ${pkgs.fzfmenu}/bin/fzfmenu "$@"
    '';
  };

  krebs.xresources.enable = true;
  lass.screenlock.enable = true;

  lass.klem = {
    kpaste.script = pkgs.writeDash "kpaste-wrapper" ''
      ${pkgs.kpaste}/bin/kpaste \
        | ${pkgs.coreutils}/bin/tail -1 \
        | ${pkgs.coreutils}/bin/tr -d '\r\n'
    '';
    go = {
      target = "STRING";
      script = "${pkgs.goify}/bin/goify";
    };
    "go.lassul.us" = {
      target = "STRING";
      script = pkgs.writeDash "go.lassul.us" ''
        export GO_HOST='go.lassul.us'
        ${pkgs.goify}/bin/goify
      '';
    };
    qrcode = {
      target = "image";
      script = pkgs.writeDash "zbar" ''
        ${pkgs.zbar}/bin/zbarimg -q -
      '';
    };
    ocr = {
      target = "image";
      script = pkgs.writeDash "gocr" ''
        ${pkgs.netpbm}/bin/pngtopnm - \
          | ${pkgs.gocr}/bin/gocr -
      '';
    };
  };
}
