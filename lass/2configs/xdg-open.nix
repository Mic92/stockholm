{ config, pkgs, lib, ... }: with import <stockholm/lib>; let

  xdg-open-wrapper = pkgs.writeDashBin "xdg-open" ''
    /run/wrappers/bin/sudo -u lass ${xdg-open} "$@"
  '';

  xdg-open = pkgs.writeBash "xdg-open" ''
    set -e
    FILE="$1"
    mime=

    case "$FILE" in
      http://*|https://*)
        mime=text/html
        ;;
      mailto:*)
        mime=special/mailaddress
        ;;
      magnet:*)
        mime=application/x-bittorrent
        ;;
      irc:*)
        mime=x-scheme-handler/irc
        ;;
      *)
        # it’s a file

        # strip possible protocol
        FILE=''${FILE#file://}
        mime=''$(file -E --brief --mime-type "$FILE") \
          || (echo "$mime" 1>&2; exit 1)
          # ^ echo the error message of file
        ;;
    esac

    case "$mime" in
      special/mailaddress)
        urxvtc --execute vim "$FILE" ;;
      ${optionalString (hasAttr "browser" config.lass) ''
      text/html)
        ${config.lass.browser.select}/bin/browser-select "$FILE" ;;
      text/xml)
        ${config.lass.browser.select}/bin/browser-select "$FILE" ;;
      ''}
      text/*)
        urxvtc --execute vim "$FILE" ;;
      image/*)
        sxiv "$FILE" ;;
      application/x-bittorrent)
        env DISPLAY=:0 transgui "$FILE" ;;
      application/pdf)
        zathura "$FILE" ;;
      inode/directory)
        sudo -u lass -i urxvtc --execute mc "$FILE" ;;
      *)
        # open dmenu and ask for program to open with
        $(dmenu_path | dmenu) "$FILE";;
    esac
  '';
in {
  environment.systemPackages = [ xdg-open-wrapper ];

  security.sudo.extraConfig = ''
    cr ALL=(lass) NOPASSWD: ${xdg-open} *
  '';
}
