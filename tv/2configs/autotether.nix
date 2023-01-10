{ config, pkgs, ... }: let
  cfg.serial = "17e064850405";
in {
  systemd.services.usb_tether.serviceConfig = {
    SyslogIdentifier = "usb_tether";
    ExecStartPre = "${pkgs.android-tools}/bin/adb -s ${cfg.serial} wait-for-device";
    ExecStart = "${pkgs.android-tools}/bin/adb -s ${cfg.serial} shell svc usb setFunctions rndis";
  };
  services.udev.extraRules = /* sh */ ''
    ACTION=="add", SUBSYSTEM=="net", KERNEL=="usb*", NAME="android"

    ACTION=="add", SUBSYSTEM=="usb", ATTR{serial}=="${cfg.serial}", \
    TAG+="systemd", ENV{SYSTEMD_WANTS}="usb_tether.service"
  '';
  systemd.network.networks.android = {
    matchConfig.Name = "android";
    DHCP = "yes";
  };
}
