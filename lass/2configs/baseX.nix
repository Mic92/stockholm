{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;
  xmonad-lass = pkgs.callPackage <stockholm/lass/5pkgs/custom/xmonad-lass> { inherit config; };
in {
  imports = [
    ./mpv.nix
    ./power-action.nix
    ./copyq.nix
    ./urxvt.nix
    ./xdg-open.nix
    ./yubikey.nix
    {
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
      security.rtkit.enable = true;
      sound.enableOSSEmulation = false;
    }
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

  users.extraUsers.mainUser.extraGroups = [ "audio" "video" ];

  time.timeZone = "Europe/Berlin";

  programs.ssh.agentTimeout = "10m";
  programs.ssh.startAgent = false;
  services.openssh.forwardX11 = true;

  environment.systemPackages = with pkgs; [
    acpi
    acpilight
    ag
    cabal2nix
    dic
    dmenu
    font-size
    fzfmenu
    gitAndTools.qgit
    git-preview
    gnome3.dconf
    lm_sensors
    ncdu
    nix-index
    nix-review
    nmap
    pavucontrol
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
  ];

  fonts.fonts = with pkgs; [
    hack-font
    hasklig
    symbola
    xlibs.fontschumachermisc
  ];

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
    displayManager.lightdm.enable = true;
    windowManager.default = "xmonad";
    windowManager.session = [{
      name = "xmonad";
      start = ''
        ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
        ${pkgs.systemd}/bin/systemctl --user start xmonad
        exec ${pkgs.coreutils}/bin/sleep infinity
      '';
    }];
  };

  systemd.user.services.xmonad = {
    environment = {
      DISPLAY = ":${toString config.services.xserver.display}";
      RXVT_SOCKET = "%t/urxvtd-socket";
      XMONAD_DATA_DIR = "/tmp";
    };
    serviceConfig = {
      SyslogIdentifier = "xmonad";
      ExecStart = "${xmonad-lass}/bin/xmonad";
      ExecStop = "${xmonad-lass}/bin/xmonad --shutdown";
    };
    restartIfChanged = false;
  };

  nixpkgs.config.packageOverrides = super: {
    dmenu = pkgs.writeDashBin "dmenu" ''
      ${pkgs.fzfmenu}/bin/fzfmenu "$@"
    '';
  };

  krebs.xresources.enable = true;
  lass.screenlock.enable = true;
}
