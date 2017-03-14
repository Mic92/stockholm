{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  copyqConfig = pkgs.writeDash "copyq-config" ''
    ${pkgs.copyq}/bin/copyq config check_clipboard true
    ${pkgs.copyq}/bin/copyq config check_selection true
    ${pkgs.copyq}/bin/copyq config copy_clipboard true
    ${pkgs.copyq}/bin/copyq config copy_selection true

    ${pkgs.copyq}/bin/copyq config activate_closes true
    ${pkgs.copyq}/bin/copyq config clipboard_notification_lines 0
    ${pkgs.copyq}/bin/copyq config clipboard_tab clipboard
    ${pkgs.copyq}/bin/copyq config disable_tray true
    ${pkgs.copyq}/bin/copyq config hide_tabs true
    ${pkgs.copyq}/bin/copyq config hide_toolbar true
    ${pkgs.copyq}/bin/copyq config item_popup_interval true
    ${pkgs.copyq}/bin/copyq config maxitems 1000
    ${pkgs.copyq}/bin/copyq config move true
    ${pkgs.copyq}/bin/copyq config text_wrap true
  '';
in {
  systemd.services.copyq = {
    wantedBy = [ "multi-user.target" ];
    requires = [ "display-manager.service" ];
    environment = {
      DISPLAY = ":0";
    };
    serviceConfig = {
      SyslogIdentifier = "copyq";
      ExecStart = "${pkgs.copyq}/bin/copyq";
      ExecStartPost = copyqConfig;
      Restart = "always";
      RestartSec = "2s";
      StartLimitBurst = 0;
      User = "lass";
    };
  };
}
