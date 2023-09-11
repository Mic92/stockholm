with import ./lib;
{ config, pkgs, ... }: let
  cfg = {
    pin = "@${config.krebs.secret.directory}/o2.pin";
    ttys.ppp = "/dev/ttyACM0";
    ttys.com = "/dev/ttyACM1";
  };
in {
  assertions = [
    {
      assertion =
        config.networking.resolvconf.enable ||
        config.networking.useNetworkd;
      message = "ppp configuration needs resolvconf or networkd";
    }
  ];
  environment.etc."ppp/ip-up".source = pkgs.writeDash "ppp.ip-up" ''
    ${pkgs.openresolv}/bin/resolvconf -a "$IFNAME" < /etc/ppp/resolv.conf
  '';
  environment.etc."ppp/ip-down".source = pkgs.writeDash "ppp.ip-down" ''
    ${pkgs.openresolv}/bin/resolvconf -fd "$IFNAME"
  '';
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
    (pkgs.writeDashBin "modem-send" ''
      # usage: modem-send ATCOMMAND
      set -efu
      tty=${lib.shell.escape cfg.ttys.com}
      exec <"$tty"
      printf '%s\r\n' "$1" >"$tty"
      ${pkgs.gnused}/bin/sed -E '
        /^OK\r?$/q
        /^ERROR\r?$/q
      '
    '')
  ];
}
