{ coreutils, dash, gnused, fzf, pass-otp, runCommand, rxvt_unicode, stdenv, utillinux, xdotool }:

runCommand "fzmenu" {
} /* sh */ ''
  mkdir $out

  cp -r ${./bin} $out/bin

  substituteInPlace $out/bin/otpmenu \
      --replace '#! /bin/sh' '#! ${dash}/bin/dash' \
      --replace '#PATH=' PATH=${stdenv.lib.makeBinPath [
        coreutils
        dash
        fzf
        gnused
        pass-otp
        rxvt_unicode
        utillinux
        xdotool
      ]}

  substituteInPlace $out/bin/passmenu \
      --replace '#! /bin/sh' '#! ${dash}/bin/dash' \
      --replace '#PATH=' PATH=${stdenv.lib.makeBinPath [
        coreutils
        dash
        fzf
        gnused
        pass-otp
        rxvt_unicode
        utillinux
        xdotool
      ]}
''
