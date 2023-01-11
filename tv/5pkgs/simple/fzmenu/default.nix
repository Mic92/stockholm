{ lib, pkgs, stdenv }:

let
  terminal = pkgs.writeDashBin "terminal" ''
    # usage: terminal COMMAND [ARGS...]
    exec ${pkgs.alacritty-tv}/bin/alacritty \
        --profile=fzmenu \
        --class AlacrittyFzmenuFloat \
        -e "$@"
  '';
in

pkgs.runCommand "fzmenu" {
} /* sh */ ''
  mkdir $out

  cp -r ${./bin} $out/bin

  substituteInPlace $out/bin/otpmenu \
      --replace '#! /bin/sh' '#! ${pkgs.dash}/bin/dash' \
      --replace '#PATH=' PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.dash
        pkgs.fzf
        pkgs.gnused
        (pkgs.pass.withExtensions (ext: [
          ext.pass-otp
        ]))
        pkgs.utillinux
        pkgs.xdotool
        terminal
      ]}

  substituteInPlace $out/bin/passmenu \
      --replace '#! /bin/sh' '#! ${pkgs.dash}/bin/dash' \
      --replace '#PATH=' PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.dash
        pkgs.fzf
        pkgs.gnused
        (pkgs.pass.withExtensions (ext: [
          ext.pass-otp
        ]))
        pkgs.utillinux
        pkgs.xdotool
        terminal
      ]}
''
