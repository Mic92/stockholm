{ pkgs, ... }: {

  # usage: ppp dial netzclub

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
      TIMEOUT 6
      ''' 'ATQ0'
      'OK-AT-OK' 'ATZ'
      TIMEOUT 3
      'OK\d-AT-OK' 'ATI'
      'OK' 'ATZ'
      'OK' 'ATQ0 V1 E1 S0=0 &C1 &D2 +FCLASS=0'
      'OK' 'ATDT*99***1#'
      TIMEOUT 30
      CONNECT '''
    ''}
  '';

  environment.systemPackages = [
    ppp
  ];

}
