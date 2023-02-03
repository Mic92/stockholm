{ pkgs, stockholm, ... }@args:
with stockholm.lib;

let
  # config cannot be declared in the input attribute set because that would
  # cause callPackage to inject the wrong config.  Instead, get it from ...
  # via args.
  config = args.config or {};
in

  pkgs.symlinkJoin {
    name = "flameshot-once-wrapper";
    paths = [
      (pkgs.writeDashBin "flameshot-once" ''
        export PATH=${makeBinPath [
          pkgs.flameshot
          pkgs.qt5.qtbase
          pkgs.xclip
          pkgs.xwaitforwindow
        ]}
        ${optionalString (config != null) /* sh */ ''
          . ${import ./profile.nix { inherit config pkgs; }}
        ''}
        exec ${pkgs.haskellPackages.flameshot-once}/bin/flameshot-once "$@"
      '')
      pkgs.haskellPackages.flameshot-once
    ];
  }
