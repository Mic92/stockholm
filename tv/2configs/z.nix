{ config, lib, pkgs, ... }:

with lib;

{
  krebs.per-user.z.packages = [
    pkgs.cr
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
