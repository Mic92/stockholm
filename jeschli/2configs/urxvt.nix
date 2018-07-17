{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  options.jeschliFontSize = mkOption {
    type = types.int;
    default = 12;
  };
  config = {
    services.urxvtd.enable = true;
    krebs.xresources.enable = true;
    krebs.xresources.resources.urxvt = ''
    *foreground: rgb:a8/a8/a8
    *background: rgb:00/00/00
    *faceName: DejaVu Sans Mono
    *faceSize: ${toString config.jeschliFontSize}
    *color0: rgb:00/00/00
    *color1: rgb:a8/00/00
    *color2: rgb:00/a8/00
    *color3: rgb:a8/54/00
    *color4: rgb:26/8b/d2
    *color5: rgb:a8/00/a8
    *color6: rgb:00/a8/a8
    *color7: rgb:a8/a8/a8
    *color8: rgb:54/54/54
    *color9: rgb:fc/54/54
    *color10: rgb:54/fc/54
    *color11: rgb:fc/fc/54
    *color12: rgb:54/54/fc
    *color13: rgb:fc/54/fc
    *color14: rgb:54/fc/fc
    *color15: rgb:fc/fc/fc

    URxvt*scrollBar:                      false
    URxvt*urgentOnBell:                   true
    URxvt*font: xft:DejaVu Sans Mono:pixelsize=${toString config.jeschliFontSize}
    URXvt*faceSize: ${toString config.jeschliFontSize}
    '';
  };
}
