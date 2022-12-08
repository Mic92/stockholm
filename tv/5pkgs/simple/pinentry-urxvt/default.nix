{ pkgs, ... }@args:

let
  lib = import <stockholm/lib>;

  # config cannot be declared in the input attribute set because that would
  # cause callPackage to inject the wrong config.  Instead, get it from ...
  # via args.
  config = args.config or {};

  cfg = eval.config;

  eval = lib.evalModules {
    modules = lib.singleton {
      _file = toString ./default.nix;
      imports = lib.singleton config;
      options = {
        appName = lib.mkOption {
          default = "pinentry-urxvt";
          type = lib.types.str;
        };
        display = lib.mkOption {
          default = null;
          type = lib.types.nullOr lib.types.str;
        };
        xwud.className = lib.mkOption {
          default = "PinentryUrxvtXwudFloat";
          type = lib.types.str;
        };
      };
    };
  };


in

  # pinentry-urxvt - A mechanism for PIN entry utilizing rxvt-unicode
  #
  # This spawns a PIN entry terminal on top of a tinted screenshot of the
  # current display's root window.  The display for spawning the terminal can
  # be predefined, in which case both the current and the predefined display
  # will show the screenshot.
  #
  # The purpose of the screenshot, aside from looking nice, is to prevent entry
  # of the PIN into the wrong window, e.g. by accidentally moving the cursor
  # while typing.  If necessary, the screenshot can be closed by sending 'q',
  # 'Q', or ctrl-c while its focused.
  #
  pkgs.write "pinentry-urxvt" {
    "/bin/pinentry".link = pkgs.writeDash "pinentry-urxvt-wrapper" ''
      set -efu

      trap cleanup EXIT

      cleanup() {
        rm "$screenshot"
        # Kill process group in order to kill screenshot windows.
        ${pkgs.utillinux}/bin/kill 0
      }

      screenshot=$(${pkgs.coreutils}/bin/mktemp -t pinentry-urxvt.screenshot.XXXXXXXX)

      ${pkgs.xorg.xwd}/bin/xwd -root |
      ${pkgs.imagemagick}/bin/convert xwd:- -fill \#424242 -colorize 80% xwd:"$screenshot"

      show_screenshot() {
        ${pkgs.exec "pinentry-urxvt.show_screenshot" {
            filename = "${pkgs.xorg.xwud}/bin/xwud";
            argv = [
              cfg.xwud.className
              "-noclick"
            ];
        }} < "$screenshot" &
        wait_for_screenshot $!
      }

      # Wait for the xwud window by trying to intercept the call to munmap().
      # If it cannot be intercepted within 0.1s, assume that attaching strace
      # wasn't fast enough or xwud doesn't call munmap() anymore.  In either
      # case fall back to search the window by class name, assuming there can
      # be only one per display.
      wait_for_screenshot() {
        if ! \
          ${pkgs.coreutils}/bin/timeout 0.1 \
          ${pkgs.strace}/bin/strace -p "$1" -e munmap 2>&1 |
          read -r _
        then
          until ${pkgs.xdotool}/bin/xdotool search \
                    --classname ${lib.shell.escape cfg.xwud.className}
          do
            ${pkgs.coreutils}/bin/sleep 0.1
          done
        fi
      }

      show_screenshot

      ${lib.optionalString (cfg.display != null) /* sh */ ''
        if test "$DISPLAY" != ${lib.shell.escape cfg.display}; then
          export DISPLAY=${lib.shell.escape cfg.display}
          show_screenshot
        fi
      ''}

      exec 3<&0 4>&1 5>&2
      ${pkgs.rxvt_unicode}/bin/urxvt \
        -name ${lib.shell.escape cfg.appName} \
        -e ${pkgs.writeDash "pinentry-urxvt-tty" ''
          set -efu
          exec 2>&5
          TTY=$(${pkgs.coreutils}/bin/tty)
          while read -r line <&3; do
            case $line in
              'OPTION ttyname='*)
                echo "OPTION ttyname=$TTY"
                ;;
              *)
                echo "$line"
            esac
          done | ${pkgs.pinentry.tty}/bin/pinentry-tty "$@" >&4
        ''} \
        "$@"
    '';
  }
