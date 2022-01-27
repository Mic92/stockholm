{ pkgs, ... }: {
  systemd.user.sockets.urxvtd = {
    wantedBy = [ "sockets.target" ];
    socketConfig.ListenStream = "%t/urxvtd";
  };
  systemd.user.services.urxvtd = {
    restartIfChanged = false;
    environment = {
      RXVT_SOCKET = "%t/urxvtd";
    };
    serviceConfig = {
      ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
    };
  };
}
