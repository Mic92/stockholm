{ lib, stdenv
, runCommand
, coreutils, dash, gnused, fzf, pass-otp, rxvt_unicode, utillinux, xdotool
}:

runCommand "fzmenu" {
} /* sh */ ''
  mkdir $out

  cp -r ${./bin} $out/bin

  substituteInPlace $out/bin/otpmenu \
      --replace '#! /bin/sh' '#! ${dash}/bin/dash' \
      --replace '#PATH=' PATH=${lib.makeBinPath [
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
      --replace '#PATH=' PATH=${lib.makeBinPath [
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
