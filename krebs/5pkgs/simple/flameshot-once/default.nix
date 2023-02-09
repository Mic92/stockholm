{ name ? "flameshot-once", pkgs, ... }@args:
with pkgs.stockholm.lib;

let
  # config cannot be declared in the input attribute set because that would
  # cause callPackage to inject the wrong config.  Instead, get it from ...
  # via args.
  config = args.config or {};

  cfg = evalModulesConfig (singleton {
    _file = toString ./default.nix;
    _module.args.pkgs = pkgs;
    imports = [
      config
      ./config.nix
    ];
  });
in

pkgs.symlinkJoin {
  inherit name;
  paths = [
    (pkgs.write "flameshot-once" {
      "/bin/flameshot-once" = {
        executable = true;
        text = /* sh */ ''
          #! ${pkgs.dash}/bin/dash
          export PATH=${makeBinPath [
            pkgs.qt5.qtbase
          ]}:''${PATH+:$PATH}
          ${optionalString (config != null) /* sh */ ''
            export XDG_CONFIG_HOME=${placeholder "out"}/etc
            ${optionalString cfg.imgur.enable /* sh */ ''
              export IMGUR_CREATE_URL=${shell.escape cfg.imgur.createUrl}
              export IMGUR_DELETE_URL=${shell.escape cfg.imgur.deleteUrl}
              ${optionalString cfg.imgur.xdg-open.enable /* sh */ ''
                export PATH=${placeholder "out"}/lib/imgur/bin''${PATH+:$PATH}
              ''}
            ''}
          ''}
          ${cfg.package}/bin/flameshot &
          exec ${cfg.package}/bin/flameshot gui
        '';
      };
      "/etc/flameshot/flameshot.ini".text =
        lib.generators.toINI {} (stripAttr cfg.settings);
      ${if cfg.imgur.enable then "/lib/imgur/bin/xdg-open" else null} = {
        executable = true;
        text = /* sh */ ''
          #! ${pkgs.dash}/bin/dash
          set -efu
          uri=$1
          prefix=$(${pkgs.coreutils}/bin/dirname "$uri")
          case $prefix in
            (${shell.escape cfg.imgur.xdg-open.createPrefix})
              echo "opening image in browser: $uri" >&2
              exec ${config.imgur.xdg-open.browser} "$uri"
              ;;
            (${shell.escape cfg.imgur.xdg-open.deletePrefix})
              echo "deleting image: $uri" >&2
              exec ${pkgs.curl}/bin/curl -fsS -X DELETE "$uri"
              ;;
            (*)
              echo "don't know how to open URI: $uri" >&2
              exit 1
          esac
        '';
      };
    })
  ];
}
// {
  dev = pkgs.write "flameshot-once-tools" {
    "/bin/get-buttonTypes" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        indent=$(${placeholder "out"}/bin/indent-of buttonTypes)
        src=${cfg.package.src}/src/tools/capturetool.h
        ${pkgs.coreutils}/bin/cat "$src" |
        ${pkgs.gnused}/bin/sed -nr '
          s/^\s*(TYPE_\S+)\s*=\s*([0-9]+),/\1 = \2;/p
        ' |
        ${placeholder "out"}/bin/prefix "  $indent"
      '';
    };
    "/bin/get-iterableButtonTypes" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        indent=$(${placeholder "out"}/bin/indent-of iterableButtonTypes)
        src=${cfg.package.src}/src/widgets/capture/capturetoolbutton.cpp
        ${pkgs.coreutils}/bin/cat "$src" |
        ${pkgs.gnused}/bin/sed -n '/\<iterableButtonTypes = {/,/^}/p' |
        ${pkgs.gcc}/bin/cpp |
        ${pkgs.coreutils}/bin/tr , \\n |
        ${pkgs.gnused}/bin/sed -rn 's/^ *CaptureTool::(TYPE_[A-Z_]+).*/"\1"/p' |
        ${pkgs.coreutils}/bin/sort |
        ${placeholder "out"}/bin/prefix "  $indent"
      '';
    };
    "/bin/get-recognizedGeneralOptions" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        src=${cfg.package.src}/src/utils/confighandler.cpp
        ${pkgs.coreutils}/bin/cat "$src" |
        ${pkgs.gnused}/bin/sed -n '/\<recognizedGeneralOptions = {/,/^};/p' |
        ${pkgs.gcc}/bin/cpp |
        ${pkgs.gnugrep}/bin/grep -F OPTION |
        ${pkgs.coreutils}/bin/sort
      '';
    };
    "/bin/get-Shortcuts" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        indent=$(${placeholder "out"}/bin/indent-of Shortcuts)
        src=${cfg.package.src}/src/utils/confighandler.cpp
        ${pkgs.coreutils}/bin/cat "$src" |
        ${pkgs.gnused}/bin/sed -n '/recognizedShortcuts = {/,/^};/p ' |
        ${pkgs.gcc}/bin/cpp |
        ${pkgs.gnused}/bin/sed -nr 's/^\s*SHORTCUT\("(TYPE_[^"]+).*/"\1"/p' |
        ${pkgs.coreutils}/bin/sort |
        ${placeholder "out"}/bin/prefix "  $indent"
      '';
    };
    "/bin/indent-of" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        # usage: indent-of NAME NIX_FILE
        exec ${pkgs.gawk}/bin/awk -v name="$1" '
          $1 == name && $2 == "=" {
            sub("[^ ].*", "")
            print
          }
        ' ${./config.nix}
      '';
    };
    "/bin/prefix" = {
      executable = true;
      text = /* sh */ ''
        #! ${pkgs.dash}/bin/dash
        ${pkgs.gawk}/bin/awk -v prefix="$1" '{ print prefix $0 }'
      '';
    };
  };
}
