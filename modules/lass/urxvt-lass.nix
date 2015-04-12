{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  imports = [
    ./urxvtd.nix
  ];

  services.urxvtd = {
    enable = true;
    users = [ mainUser.name ];
    urxvtPackage = pkgs.rxvt_unicode_with-plugins;
    xresources = ''
      URxvt*scrollBar:                      false
      URxvt*urgentOnBell:                   true
      URxvt*font:                           -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-*
      URxvt*boldFont:                       -*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-*
      URxvt.perl-ext-common:      default,clipboard,url-select,keyboard-select
      URxvt.url-select.launcher:  browser-select
      URxvt.url-select.underline: true
      URxvt.keysym.M-u:           perl:url-select:select_next
      URxvt.keysym.M-Escape:      perl:keyboard-select:activate
      URxvt.keysym.M-s:           perl:keyboard-select:search
      
      URxvt.intensityStyles: false
      
      !solarized colors
      URxvt*fading:                         5
      URxvt*background:                     #002b36
      URxvt*foreground:                     #657b83
      URxvt*fadeColor:                      #002b36
      URxvt*cursorColor:                    #93a1a1
      URxvt*pointerColorBackground:         #586e75
      URxvt*pointerColorForeground:         #93a1a1
      URxvt*colorUL:                        #859900
      URxvt*colorBD:                        #268bd2
      URxvt*color0:                         #073642
      URxvt*color8:                         #002b36
      URxvt*color1:                         #dc322f
      URxvt*color9:                         #cb4b16
      URxvt*color2:                         #859900
      URxvt*color10:                        #586e75
      URxvt*color3:                         #b58900
      URxvt*color11:                        #657b83
      URxvt*color4:                         #268bd2
      URxvt*color12:                        #839496
      URxvt*color5:                         #d33682
      URxvt*color13:                        #6c71c4
      URxvt*color6:                         #2aa198
      URxvt*color14:                        #93a1a1
      URxvt*color7:                         #eee8d5
      URxvt*color15:                        #fdf6e3
    '';
  };
}
