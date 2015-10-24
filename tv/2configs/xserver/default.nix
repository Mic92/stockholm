{ config, lib, pkgs, ... }@args:

with lib;

let
  # TODO krebs.build.user
  user = config.users.users.tv;

  out = {
    services.xserver.display = 11;
    services.xserver.tty = 11;

    services.xserver.synaptics = {
      enable = true;
      twoFingerScroll = true;
      accelFactor = "0.035";
    };

    fonts.fonts = [
      pkgs.xlibs.fontschumachermisc
    ];

    systemd.services.urxvtd = {
      wantedBy = [ "multi-user.target" ];
      reloadIfChanged = true;
      serviceConfig = {
        ExecReload = need-reload "urxvtd.service";
        ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
        Restart = "always";
        RestartSec = "2s";
        StartLimitBurst = 0;
        User = user.name;
      };
    };

    environment.systemPackages = [
      pkgs.slock
    ];

    security.setuidPrograms = [
      "slock"
    ];

    systemd.services.display-manager = mkForce {};

    services.xserver.enable = true;
    systemd.services.xmonad = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "xserver.service" ];
      serviceConfig = {
        ExecStart = "${xmonad}/bin/xmonad";
        User = user.name;
        WorkingDirectory = user.home;
      };
    };

    systemd.services.xserver = {
      after = [
        "systemd-udev-settle.service"
        "local-fs.target"
        "acpid.service"
      ];
      reloadIfChanged = true;
      environment = xserver-environment;
      serviceConfig = {
        ExecReload = need-reload "xserver.service";
        ExecStart = "${xserver}/bin/xserver";
      };
    };
  };

  xmonad = let
    pkg = pkgs.haskellPackages.callPackage src {};
    src = pkgs.runCommand "xmonad-package" {} ''
      ${pkgs.cabal2nix}/bin/cabal2nix ${./xmonad} > $out
    '';
  in pkgs.writeScriptBin "xmonad" ''
    #! /bin/sh
    set -efu
    export DISPLAY; DISPLAY=:${toString config.services.xserver.display}
    export PATH; PATH=${makeSearchPath "bin" [
      pkgs.rxvt_unicode
    ]}
    settle() {(
      # Use PATH for a clean journal
      command=''${1##*/}
      PATH=''${1%/*}; export PATH
      shift
      until "$command" "$@"; do
        ${pkgs.coreutils}/bin/sleep 1
      done
    )&}
    settle ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
    settle ${pkgs.xorg.xrdb}/bin/xrdb -merge ${import ./Xresources.nix args}
    settle ${pkgs.xorg.xsetroot}/bin/xsetroot -solid '#1c1c1c'
    exec ${pkg}/bin/xmonad
  '';

  xserver-environment = {
    XKB_BINDIR = "${pkgs.xorg.xkbcomp}/bin"; # Needed for the Xkb extension.
    XORG_DRI_DRIVER_PATH = "/run/opengl-driver/lib/dri"; # !!! Depends on the driver selected at runtime.
    LD_LIBRARY_PATH = concatStringsSep ":" (
      [ "${pkgs.xorg.libX11}/lib" "${pkgs.xorg.libXext}/lib" ]
      ++ concatLists (catAttrs "libPath" config.services.xserver.drivers));
  };
      
  xserver = pkgs.writeScriptBin "xserver" ''
    #! /bin/sh
    set -efu
    exec ${pkgs.xorg.xorgserver}/bin/X \
        :${toString config.services.xserver.display} \
        vt${toString config.services.xserver.tty} \
        -config ${import ./xserver.conf.nix args} \
        -logfile /var/log/X.${toString config.services.xserver.display}.log \
        -nolisten tcp \
        -xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb \
  '';

  need-reload = s: let
    pkg = pkgs.writeScriptBin "need-reload" ''
      #! /bin/sh
      echo "$*"
    '';
  in "${pkg}/bin/need-reload ${s}";

in out
