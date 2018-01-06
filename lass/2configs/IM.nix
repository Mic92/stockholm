with (import <stockholm/lib>);
{ config, lib, pkgs, ... }:

let
  tmux = pkgs.writeDash "tmux" ''
    exec ${pkgs.tmux}/bin/tmux -f ${pkgs.writeText "tmux.conf" ''
      set-option -g prefix `
      unbind-key C-b
      bind ` send-prefix

      set-option -g status off
      set-option -g default-terminal screen-256color

      #use session instead of windows
      bind-key c new-session
      bind-key p switch-client -p
      bind-key n switch-client -n
      bind-key C-s switch-client -l
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
      lass.pubkey
      lass-shodan.pubkey
      lass-icarus.pubkey
      lass-android.pubkey
    ];
  };

  # mosh
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p udp --dport 60000:61000"; target = "ACCEPT";}
    { predicate = "-p tcp --dport 9999"; target = "ACCEPT";}
  ];

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
      ExecStart = "${tmux} -2 new-session -d -s IM ${pkgs.weechat}/bin/weechat";
      ExecStop = "${tmux} kill-session -t IM";
    };
  };
}
