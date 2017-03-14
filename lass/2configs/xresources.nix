{ config, pkgs, ... }:

with import <stockholm/lib>;

let

  xresources = pkgs.writeText "Xresources" ''
    URxvt*scrollBar: false
    URxvt*urgentOnBell: true
    URxvt*SaveLines: 4096
    URxvt*font: -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1
    URxvt*boldFont: -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1

    ! ref https://github.com/muennich/urxvt-perls
    URxvt.perl-lib: ${pkgs.urxvt_perls}/lib/urxvt/perl
    URxvt.perl-ext-common:      default,clipboard,url-select,keyboard-select
    URxvt.url-select.launcher:  ${config.lass.browser.select}/bin/browser-select
    URxvt.url-select.underline: true
    URxvt.keysym.M-u:           perl:url-select:select_next
    URxvt.keysym.M-Escape:      perl:keyboard-select:activate
    URxvt.keysym.M-s:           perl:keyboard-select:search

    URxvt.intensityStyles: false

    URxvt*background:    #000000
    URxvt*foreground:    #d0d7d0

    URxvt*cursorColor:   #f042b0
    URxvt*cursorColor2:  #f0b000
    URxvt*cursorBlink:   off

    URxvt*.pointerBlank: true
    URxvt*.pointerBlankDelay: 987654321
    URxvt*.pointerColor: #f042b0
    URxvt*.pointerColor2: #050505
  '';

in {
  systemd.services.xresources = {
    description = "xresources";
    wantedBy = [ "multi-user.target" ];
    after = [ "display-manager.service" ];

    environment = {
      DISPLAY = ":0";
    };

    restartIfChanged = true;

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.xorg.xrdb}/bin/xrdb -merge ${xresources}";
      Restart = "on-failure";
      User = "lass";
    };
  };
}
