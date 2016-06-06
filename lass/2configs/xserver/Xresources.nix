{ config, lib, pkgs, ... }:

with config.krebs.lib;

pkgs.writeText "Xresources" ''
  URxvt*scrollBar:                      false
  URxvt*urgentOnBell:                   true
  URxvt*font:                           -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-*
  URxvt*boldFont:                       -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-*

  ! ref https://github.com/muennich/urxvt-perls
  URxvt.perl-lib: ${pkgs.urxvt_perls}/lib/urxvt/perl
  URxvt.perl-ext-common:      default,clipboard,url-select,keyboard-select
  URxvt.url-select.launcher:  browser-select
  URxvt.url-select.underline: true
  URxvt.keysym.M-u:           perl:url-select:select_next
  URxvt.keysym.M-Escape:      perl:keyboard-select:activate
  URxvt.keysym.M-s:           perl:keyboard-select:search

  URxvt.intensityStyles: false

  URxvt*background:	#050505
  ! URxvt*background:	#041204

  !URxvt.depth: 32
  !URxvt*background: rgba:0500/0500/0500/cccc

  ! URxvt*background:	#080810
  URxvt*foreground:	#d0d7d0
  ! URxvt*background:	black
  ! URxvt*foreground:	white
  ! URxvt*background:	rgb:00/00/40
  ! URxvt*foreground:	rgb:a0/a0/d0
  ! XTerm*cursorColor:	rgb:00/00/60
  URxvt*cursorColor:	#f042b0
  URxvt*cursorColor2:	#f0b000
  URxvt*cursorBlink:	off
  ! URxvt*cursorUnderline: true
  ! URxvt*highlightColor: #232323
  ! URxvt*highlightTextColor: #b0ffb0

  URxvt*.pointerBlank: true
  URxvt*.pointerBlankDelay: 987654321
  URxvt*.pointerColor: #f042b0
  URxvt*.pointerColor2: #050505

  ! URxvt*color0:	#000000
  ! URxvt*color1:	#c00000
  ! URxvt*color2:	#80c070
  URxvt*color3:	#c07000
  ! URxvt*color4:	#0000c0
  URxvt*color4:	#4040c0
  ! URxvt*color5:	#c000c0
  ! URxvt*color6:	#008080
  URxvt*color7:	#c0c0c0

  URxvt*color8:	#707070
  URxvt*color9:	#ff6060
  URxvt*color10:	#70ff70
  URxvt*color11:	#ffff70
  URxvt*color12:	#7070ff
  URxvt*color13:	#ff50ff
  URxvt*color14:	#70ffff
  URxvt*color15:	#ffffff

''
