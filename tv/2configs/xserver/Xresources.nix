{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

pkgs.writeText "Xresources" /* xdefaults */ ''
  URxvt*cutchars: "\\`\"'&()*,;<=>?@[]^{|}‘’"
  URxvt*eightBitInput: false
  URxvt*font: -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1
  URxvt*boldFont: -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1
  URxvt*scrollBar: false
  URxvt*background: #050505
  URxvt*foreground: #d0d7d0
  URxvt*cursorColor: #f042b0
  URxvt*cursorColor2: #f0b000
  URxvt*cursorBlink: off
  URxvt*jumpScroll: true
  URxvt*allowSendEvents: false
  URxvt*charClass: 33:48,37:48,45-47:48,64:48,38:48,61:48,63:48
  URxvt*cutNewline: False
  URxvt*cutToBeginningOfLine: False

  URxvt*color0: #232342
  URxvt*color3: #c07000
  URxvt*color4: #4040c0
  URxvt*color7: #c0c0c0
  URxvt*color8: #707070
  URxvt*color9: #ff6060
  URxvt*color10: #70ff70
  URxvt*color11: #ffff70
  URxvt*color12: #7070ff
  URxvt*color13: #ff50ff
  URxvt*color14: #70ffff
  URxvt*color15: #ffffff

  URxvt*iso14755: False

  URxvt*urgentOnBell: True
  URxvt*visualBell: True

  ! ref https://github.com/muennich/urxvt-perls
  URxvt*perl-ext: default,url-select
  URxvt*keysym.M-u: perl:url-select:select_next
  URxvt*url-select.launcher: /etc/per-user/${config.krebs.build.user.name}/bin/ff -new-tab
  URxvt*url-select.underline: true
  URxvt*colorUL: #4682B4
  URxvt.perl-lib: ${pkgs.urxvt_perls}/lib/urxvt/perl
  URxvt*saveLines: 10000

  root-urxvt*background: #230000
  root-urxvt*foreground: #e0c0c0
  root-urxvt*BorderColor: #400000
  root-urxvt*color0: #800000
''
