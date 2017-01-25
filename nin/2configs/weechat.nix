{ config, lib, pkgs, ... }:

let
  inherit (import <stockholm/lib>) genid;
in {
  krebs.per-user.chat.packages = with pkgs; [
    mosh
    weechat
    tmux
  ];

  users.extraUsers.chat = {
    home = "/home/chat";
    uid = genid "chat";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.nin.pubkey
    ];
  };
}
