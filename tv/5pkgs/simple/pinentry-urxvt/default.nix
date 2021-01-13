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
          default = ":0";
          type = lib.types.str;
        };
      };
    };
  };


in

  pkgs.write "pinentry-urxvt" {
    "/bin/pinentry".link = pkgs.writeDash "pinentry-urxvt-wrapper" ''
      set -efu
      exec 3<&0 4>&1 5>&2
      export DISPLAY=${lib.shell.escape cfg.display}
      exec ${pkgs.rxvt_unicode}/bin/urxvt \
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
