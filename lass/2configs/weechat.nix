{ config, lib, pkgs, ... }:

with lib;
{
  imports = [
    ../3modules/per-user.nix
  ];

  lass.per-user.chat.packages = [
    pkgs.weechat
    pkgs.tmux
  ];

  users.extraUsers.chat = {
    home = "/home/chat";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = map readFile [
      ../../krebs/Zpubkeys/lass.ssh.pub
    ];
  };
}
