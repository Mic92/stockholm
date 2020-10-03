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
  users.users.root.packages = [
    (pkgs.writeDashBin "connect" ''
      # usage:
      #   connect wlan
      #   connect wwan [PEERNAME]
      set -efu
      rfkill_wlan=/sys/class/rfkill/rfkill2
      rfkill_wwan=/sys/class/rfkill/rfkill1
      case $1 in
        wlan)
          ${pkgs.procps}/bin/pkill pppd || :
          echo 0 > "$rfkill_wwan"/state
          echo 1 > "$rfkill_wlan"/state
          ;;
        wwan)
          name=''${2-o2}
          echo 0 > "$rfkill_wlan"/state
          echo 1 > "$rfkill_wwan"/state
          ${pkgs.ppp}/bin/pppd call "$name" updetach
          ;;
        *)
          echo "$0: error: bad arguments: $*" >&2
          exit 1
      esac
    '')
  ];
}
