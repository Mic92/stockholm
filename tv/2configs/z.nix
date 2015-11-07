{ config, lib, pkgs, ... }:

with lib;

{
  krebs.per-user.z.packages = [
    (pkgs.writeScriptBin "cr" ''
      #! /bin/sh
      set -efu
      export LC_TIME=de_DE.utf8
      exec ${pkgs.chromium}/bin/chromium \
          --ssl-version-min=tls1 \
          --disk-cache-dir=/tmp/chromium-disk-cache_"$LOGNAME" \
          --disk-cache-size=50000000 \
          "%@"
    '')
  ];

  programs.bash.interactiveShellInit = ''
    case ''${XMONAD_SPAWN_WORKSPACE-} in
      za|zh|zj|zs)
        exec sudo -u z -i
      ;;
    esac
  '';

  security.sudo.extraConfig = "tv ALL=(z) NOPASSWD: ALL";

  users.users.z = {
    extraGroups = [
      "audio"
      "vboxusers"
      "video"
    ];
    group = "subusers";
    home = "/home/z";
    uid = 3043726074; # genid z
    useDefaultShell = true;
  };
}
