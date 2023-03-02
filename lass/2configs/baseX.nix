{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;
in {
  imports = [
    ./alacritty.nix
    ./mpv.nix
    ./power-action.nix
    ./urxvt.nix
    ./xdg-open.nix
    ./yubikey.nix
    ./pipewire.nix
    ./tmux.nix
    ./xmonad.nix
    ./themes.nix
    {
      users.users.mainUser.packages = [
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

  users.users.mainUser.extraGroups = [ "audio" "pipewire" "video" ];

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
    gitAndTools.gh
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
    rxvt-unicode
    sshvnc
    sxiv
    nsxiv
    taskwarrior
    termite
    transgui
    wirelesstools
    x11vnc
    xclip
    xephyrify
    xorg.xhost
    xsel
    zathura
    flameshot
    (pkgs.writeDashBin "screenshot" ''
      set -efu

      ${pkgs.flameshot}/bin/flameshot gui
      ${pkgs.klem}/bin/klem
    '')
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      xorg.fontschumachermisc
      inconsolata
      noto-fonts
      (iosevka.override {
        # https://typeof.net/Iosevka/customizer
        privateBuildPlan = {
          family = "Iosevka";
          spacing = "term";
          serifs = "slab";
          no-ligation = true;

          variants.design = {
            capital-j = "serifless";
            a = "double-storey-tailed";
            b = "toothless-corner";
            d = "toothless-corner-serifless";
            f = "flat-hook-tailed";
            g = "earless-corner";
            i = "hooky";
            j = "serifless";
            l = "tailed";

            m = "earless-corner-double-arch";
            n = "earless-corner-straight";
            p = "earless-corner";
            q = "earless-corner";
            r = "earless-corner";
            u = "toothless-rounded";
            y = "cursive-flat-hook";

            one = "no-base-long-top-serif";
            two = "straight-neck";
            three = "flat-top";
            four = "open";
            six = "open-contour";
            seven = "straight-serifless";
            eight = "two-circles";
            nine = "open-contour";
            tilde = "low";
            asterisk = "hex-low";
            number-sign = "upright";
            at = "short";
            dollar = "open";
            percent = "dots";
            question = "corner-flat-hooked";
          };
        };
        set = "kookiefonts";
      })
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

  services.clipmenu.enable = true;

  # synchronize all the clipboards
  systemd.user.services.autocutsel = {
    enable = true;
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = pkgs.writers.writeDash "autocutsel" ''
        ${pkgs.autocutsel}/bin/autocutsel -fork -selection PRIMARY
        ${pkgs.autocutsel}/bin/autocutsel -fork -selection CLIPBOARD
      '';
    };
  };
}
