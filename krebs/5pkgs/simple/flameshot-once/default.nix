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
