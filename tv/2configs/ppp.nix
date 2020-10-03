{ pkgs, ... }: let
  lib = import <stockholm/lib>;
  cfg = {
    pin = "@${toString <secrets/o2.pin>}";
    ttys.ppp = "/dev/ttyACM0";
  };
in {
  environment.etc."ppp/peers/o2".text = /* sh */ ''
    ${cfg.ttys.ppp}
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
    connect "${pkgs.ppp}/bin/chat ''${DEBUG+-v} -Ss -f ${pkgs.writeText "o2.chat" /* sh */ ''
      ABORT "BUSY"
      ABORT "NO CARRIER"
      REPORT CONNECT
      "*EMRDY: 1"
      ATZ OK
      AT+CFUN=1 OK
      ${cfg.pin} TIMEOUT 2 ERROR-AT-OK
      AT+CGDCONT=1,\042IP\042,\042internet\042 OK
      ATDT*99***1# CONNECT
    ''}"
  '';
}
