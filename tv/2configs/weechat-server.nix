{ config, pkgs, ... }: {
  users.users.tv.packages = [
    (pkgs.writers.writeDashBin "weechat-client" ''
      set -efu
      exec ${pkgs.tmux}/bin/tmux attach -t weechat
    '')
  ];
  systemd.services.weechat = {
    wantedBy = [ "multi-user.target" ];
    environment = {
      TERM = "rxvt-unicode-256color";
    };
    serviceConfig = {
      ExecStart = "${pkgs.tmux}/bin/tmux new -d -s weechat ${pkgs.weechat}/bin/weechat";
      OOMScoreAdjust = -1000;
      Restart = "always";
      RestartSec = "100ms";
      Type = "forking";
      StartLimitBurst = 0;
      User = "tv";
      WorkingDirectory = "/home/tv";
    };
  };
}
