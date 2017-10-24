with import <stockholm/lib>;
{ config, lib, pkgs, ... }: let

  cfg = config.tv.umts;

  umts-dial = pkgs.writeDash "umts-dial" ''
    set -euf
    ${pkgs.systemd}/bin/systemctl start umts
    trap 'cleanup; trap - EXIT INT TERM' EXIT INT TERM
    cleanup() {
      ${pkgs.systemd}/bin/systemctl stop umts
    }
    echo nameserver 8.8.8.8 >> /etc/resolv.conf
    ${pkgs.systemd}/bin/journalctl -xfu umts
  '';

  # https://github.com/NixOS/nixpkgs/issues/16113
  wvdial = let
    nixpkgs-1509 = import (pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs-channels";
      rev = "91371c2bb6e20fc0df7a812332d99c38b21a2bda";
      sha256 = "1as1i0j9d2n3iap9b471y4x01561r2s3vmjc5281qinirlr4al73";
    }) {};
  in
    nixpkgs-1509.wvdial;

in {
  options.tv.umts = {
    enable = mkEnableOption "tv.umts";
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
    pppDefaults = mkOption {
      type = types.str;
      default = ''
        noipdefault
        usepeerdns
        defaultroute
        persist
        noauth
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.etc = {
      "ppp/peers/wvdial".text = cfg.pppDefaults;
      "wvdial.conf".text = ''
        [Dialer Defaults]
        Modem = ${cfg.modem}
        ${cfg.initstrings}
        Modem Type = Analog Modem
        Baud = 460800
        phone= *99#
        Username = ${cfg.username}
        Password = ${cfg.password}
        Stupid Mode = 1
        Idle Seconds = 0
        PPPD Path = ${pkgs.ppp}/bin/pppd
      '';
    };

    krebs.per-user.tv.packages = [
      (pkgs.writeDashBin "umts" ''
        exec sudo ${umts-dial}
      '')
    ];

    security.sudo.extraConfig = ''
      tv ALL= (root) NOPASSWD: ${umts-dial}
    '';

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
}
