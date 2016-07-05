{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  cfg = config.lass.umts;

  out = {
    options.lass.umts = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "umts";
    modem = mkOption {
      type = types.str;
      default = "/dev/ttyUSB0";
    };
    initstrings = mkOption {
      type = types.str;
      default = ''
        Init1 = ATZ
        Init2 = ATQ0 V1 E1 S0=0 &C1 &D2
      '';
    };
    username = mkOption {
      type = types.str;
      default = "default";
    };
    password = mkOption {
      type = types.str;
      default = "default";
    };
  };

  nixpkgs-1509 = import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs-channels";
    rev = "91371c2bb6e20fc0df7a812332d99c38b21a2bda";
    sha256 = "1as1i0j9d2n3iap9b471y4x01561r2s3vmjc5281qinirlr4al73";
  }) {};

  wvdial = nixpkgs-1509.wvdial; # https://github.com/NixOS/nixpkgs/issues/16113

  #modem-device = "/dev/serial/by-id/usb-Lenovo_F5521gw_38214921FBBBC7B0-if09";
  modem-device = "/dev/serial/by-id/usb-HUAWEI_Technologies_HUAWEI_Mobile-if00-port0";

  # TODO: currently it is only netzclub
  umts-bin = pkgs.writeScriptBin "umts" ''
    #!/bin/sh
    set -euf
    systemctl start umts
    trap "systemctl stop umts;trap - INT TERM EXIT;exit" INT TERM EXIT
    echo nameserver 8.8.8.8 | tee -a /etc/resolv.conf
    journalctl -xfu umts
  '';

  wvdial-defaults = ''
    Modem = ${cfg.modem}
    ${cfg.initstrings}
    Modem Type = Analog Modem
    Baud = 460800
    phone= *99#
    Username = ${cfg.username}
    Password = ${cfg.password}
    Stupid Mode = 1
    Idle Seconds = 0
  '';

  imp = {
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
