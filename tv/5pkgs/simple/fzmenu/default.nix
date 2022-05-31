{ lib, pkgs, stdenv }:

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
        pkgs.rxvt_unicode
        pkgs.utillinux
        pkgs.xdotool
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
        pkgs.rxvt_unicode
        pkgs.utillinux
        pkgs.xdotool
      ]}
''
