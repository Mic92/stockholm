{ config, pkgs, ... }:

with import ./lib;

pkgs.writeText "Xmodmap" ''
  !keycode 66 = Caps_Lock
  !remove Lock = Caps_Lock
  clear Lock

  ! caps lock
  keycode  66 = Mode_switch

  keycode  13 = 4 dollar EuroSign cent
  keycode  30 = u U udiaeresis Udiaeresis
  keycode  32 = o O odiaeresis Odiaeresis
  keycode  38 = a A adiaeresis Adiaeresis
  keycode  39 = s S ssharp

  keycode  33 = p P Greek_pi Greek_PI
  keycode  40 = d D Greek_delta Greek_DELTA
  keycode  46 = l L Greek_lambda Greek_LAMBDA

  keycode  54 = c C cacute Cacute

  !                       BULLET OPERATOR
  keycode 17 = 8 asterisk U2219
  keycode 27 = r R r U211D
''
