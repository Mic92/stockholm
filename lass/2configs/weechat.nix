{ config, lib, pkgs, ... }:

let
  inherit (import <stockholm/lib>) genid;
in {
  krebs.per-user.chat.packages = with pkgs; [
    mosh
    weechat
  ];

  users.extraUsers.chat = {
    home = "/home/chat";
    uid = genid "chat";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.lass.pubkey
      config.krebs.users.lass-shodan.pubkey
      config.krebs.users.lass-icarus.pubkey
    ];
  };

  #systemd.services.chat = {
  #  description = "chat environment setup";
  #  after = [ "network.target" ];
  #  wantedBy = [ "multi-user.target" ];

  #  path = with pkgs; [
  #    weechat
  #    tmux
  #  ];

  #  restartIfChanged = true;

  #  serviceConfig = {
  #    User = "chat";
  #    Restart = "always";
  #    ExecStart = "${pkgs.tmux}/bin/tmux new -s IM weechat";
  #  };
  #};
}
