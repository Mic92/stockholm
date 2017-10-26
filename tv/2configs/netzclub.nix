{ pkgs, ... }: {

  # usage: pppd call netzclub

  environment.etc."ppp/peers/netzclub".text = ''
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
    connect "${pkgs.ppp}/bin/chat -f ${pkgs.writeText "netzclub.script" ''
      ABORT 'BUSY'
      ABORT 'NO CARRIER'
      ABORT 'VOICE'
      ABORT 'NO DIALTONE'
      ABORT 'NO DIAL TONE'
      ABORT 'NO ANSWER'
      ABORT 'DELAYED'
      REPORT CONNECT
      "" "ATDT*99#"
      CONNECT ""
    ''}"
  '';

  environment.systemPackages = [
    pkgs.ppp
  ];

}
