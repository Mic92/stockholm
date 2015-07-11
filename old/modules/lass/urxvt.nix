{ config, pkgs, ... }:

let
  inherit (config.users.extraUsers) mainUser;

in

{
  imports = [
    ./urxvtd.nix
    ./xresources.nix
  ];

  services.urxvtd = {
    enable = true;
    users = [ mainUser.name ];
    urxvtPackage = pkgs.rxvt_unicode_with-plugins;
  };
  services.xresources.enable = true;
  services.xresources.resources.urxvt = ''
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

    URxvt*background:                     #000000
    URxvt*foreground:                     #ffffff

    !change unreadable blue
    URxvt*color4:                         #268bd2
  '';
}
