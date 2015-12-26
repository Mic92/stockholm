{ config, lib, pkgs, ... }:

{
  krebs.per-user.chat.packages = [
    pkgs.weechat
    pkgs.tmux
  ];

  users.extraUsers.chat = {
    home = "/home/chat";
    uid = lib.genid "chat";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.lass.pubkey
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
