with (import <stockholm/lib>);
{ config, lib, pkgs, ... }:
let
  tmux = pkgs.writeDashBin "tmux" ''
    export TERM=xterm-256color
    exec ${pkgs.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
      set-option -g default-terminal screen-256color
    ''} "$@"
  '';
in {

  services.bitlbee = {
    enable = true;
    portNumber = 6666;
    plugins = [
      pkgs.bitlbee-facebook
      pkgs.bitlbee-steam
      pkgs.bitlbee-discord
    ];
    libpurple_plugins = [ pkgs.telegram-purple ];
  };

  users.extraUsers.chat = {
    home = "/home/chat";
    uid = genid "chat";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = with config.krebs.users; [
      jeschli.pubkey
      jeschli-bln.pubkey
      jeschli-brauerei.pubkey
      jeschli-bolide.pubkey
    ];
    packages = [ tmux ];
  };


  systemd.services.chat = {
    description = "chat environment setup";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    restartIfChanged = false;

    path = [
      pkgs.rxvt_unicode.terminfo
    ];

    serviceConfig = {
      User = "chat";
      RemainAfterExit = true;
      Type = "oneshot";
      ExecStart = "${tmux}/bin/tmux -2 new-session -d -s IM ${pkgs.weechat}/bin/weechat";
      ExecStop = "${tmux}/bin/tmux kill-session -t IM";
    };
  };
}
