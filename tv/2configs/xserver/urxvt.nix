with import ./lib;
{ config, pkgs, ... }: let
  cfg.user = config.krebs.build.user;
in {
  systemd.services.urxvtd = {
    wantedBy = [ "graphical.target" ];
    restartIfChanged = false;
    serviceConfig = {
      SyslogIdentifier = "urxvtd";
      ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
      Restart = "always";
      RestartSec = "2s";
      StartLimitBurst = 0;
      User = cfg.user.name;
    };
  };
  tv.Xresources = {
    "URxvt*cutchars" = ''"\\`\"'&()*,;<=>?@[]^{|}‘’"'';
    "URxvt*eightBitInput" = "false";
    "URxvt*font" = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
    "URxvt*boldFont" = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
    "URxvt*scrollBar" = "false";
    "URxvt*background" = "#050505";
    "URxvt*foreground" = "#d0d7d0";
    "URxvt*cursorColor" = "#f042b0";
    "URxvt*cursorColor2" = "#f0b000";
    "URxvt*cursorBlink" = "off";
    "URxvt*jumpScroll" = "true";
    "URxvt*allowSendEvents" = "false";
    "URxvt*charClass" = "33:48,37-38:48,45-47:48,61:48,63-64:48";
    "URxvt*cutNewline" = "False";
    "URxvt*cutToBeginningOfLine" = "False";

    "URxvt*color0" = "#232342";
    "URxvt*color3" = "#c07000";
    "URxvt*color4" = "#4040c0";
    "URxvt*color7" = "#c0c0c0";
    "URxvt*color8" = "#707070";
    "URxvt*color9" = "#ff6060";
    "URxvt*color10" = "#70ff70";
    "URxvt*color11" = "#ffff70";
    "URxvt*color12" = "#7070ff";
    "URxvt*color13" = "#ff50ff";
    "URxvt*color14" = "#70ffff";
    "URxvt*color15" = "#ffffff";

    "URxvt*iso14755" = "False";

    "URxvt*urgentOnBell" = "True";
    "URxvt*visualBell" = "True";

    # ref https://github.com/muennich/urxvt-perls
    "URxvt*perl-ext" = "default,url-select";
    "URxvt*keysym.M-u" = "perl:url-select:select_next";
    "URxvt*url-select.launcher" =
      "/etc/profiles/per-user/${cfg.user.name}/bin/ff -new-tab";
    "URxvt*url-select.underline" = "true";
    "URxvt*colorUL" = "#4682B4";
    "URxvt.perl-lib" = "${pkgs.urxvt_perls}/lib/urxvt/perl";
    "URxvt*saveLines" = "10000";
    "URxvt*modifier" = "mod1";

    "root-urxvt*background" = "#230000";
    "root-urxvt*foreground" = "#e0c0c0";
    "root-urxvt*BorderColor" = "#400000";
    "root-urxvt*color0" = "#800000";

    "fzmenu-urxvt*background" = "rgb:42/23/42";
    "fzmenu-urxvt*externalBorder" = "1";
    "fzmenu-urxvt*geometry" = "70x9";
    "fzmenu-urxvt*internalBorder" = "1";
  };
}
