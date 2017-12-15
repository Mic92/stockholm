{ config, pkgs, ... }:
with import <stockholm/lib>;

{
  services.urxvtd.enable = true;

  krebs.xresources.resources.urxvt = ''
    URxvt*SaveLines: 4096
    URxvt*scrollBar:            false
    URxvt*urgentOnBell:         true
    URxvt.perl-ext-common:      default,clipboard,url-select,keyboard-select

    ${optionalString (hasAttr "browser" config.lass)
      "URxvt.url-select.launcher:  ${config.lass.browser.select}/bin/browser-select"
    }

    URxvt.url-select.underline: true
    URxvt.keysym.M-u:           perl:url-select:select_next
    URxvt.keysym.M-Escape:      perl:keyboard-select:activate
    URxvt.keysym.M-s:           perl:keyboard-select:search

    URxvt.intensityStyles:      false

    URxvt*background:           #000000
    URxvt*foreground:           #ffffff

    !change unreadable blue
    URxvt*color4:               #268bd2

    URxvt*color0:               #232342
  '';
}
