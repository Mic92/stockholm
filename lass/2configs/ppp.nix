{ pkgs, ... }: {

  # usage: pppd call default

  environment.etc."ppp/peers/default".text = ''
    /dev/ttyACM2
    921600
    crtscts
    defaultroute
    holdoff 10
    lock
    maxfail 0
    noauth
    nodetach
    noipdefault
    passive
    persist
    usepeerdns
    connect "${pkgs.ppp}/bin/chat -f ${pkgs.writeText "default.chat" ''
      ABORT "BUSY"
      ABORT "NO CARRIER"
      REPORT CONNECT
      "" "ATDT*99#"
      CONNECT
    ''}"
  '';

  environment.systemPackages = [
    pkgs.ppp
  ];

}
