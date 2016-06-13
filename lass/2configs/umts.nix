{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  nixpkgs-1509 = import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs-channels";
    rev = "91371c2bb6e20fc0df7a812332d99c38b21a2bda";
    sha256 = "1as1i0j9d2n3iap9b471y4x01561r2s3vmjc5281qinirlr4al73";
  }) {};

  wvdial = nixpkgs-1509.wvdial; # https://github.com/NixOS/nixpkgs/issues/16113

  modem-device = "/dev/serial/by-id/usb-Lenovo_F5521gw_38214921FBBBC7B0-if09";

  # TODO: currently it is only netzclub
  umts-bin = pkgs.writeScriptBin "umts" ''
    #!/bin/sh
    set -euf
    systemctl stop wpa_supplicant
    systemctl start umts
    trap "systemctl stop umts && systemctl start wpa_supplicant;trap - INT TERM EXIT;exit" INT TERM EXIT
    echo nameserver 8.8.8.8 | tee -a /etc/resolv.conf
    journalctl -xfu umts
  '';

  wvdial-defaults = ''
    Modem = ${modem-device}
    Init1 = AT+CFUN=1
    Init2 = AT+CGDCONT=1,"IP","pinternet.interkom.de","",0,0
    Stupid mode = 1
    phone= *99#
    Username = netzclub
    Password = netzclub
  '';


  out = {
    environment.shellAliases = {
      umts = "sudo ${umts-bin}/bin/umts";
    };

    security.sudo.extraConfig = ''
      lass ALL= (root) NOPASSWD: ${umts-bin}/bin/umts
    '';

    environment.wvdial.dialerDefaults = wvdial-defaults;

    systemd.services.umts = {
      description = "UMTS wvdial Service";
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "10s";
        ExecStart = "${wvdial}/bin/wvdial -n";
      };
    };
  };
in out

