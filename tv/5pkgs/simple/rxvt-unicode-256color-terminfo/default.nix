# This package is mainly intended for cross-built systems for which we cannot
# or don't want to build pkgs.rxvt_unicode for some reason.
#
# ${./rxvt-unicode-256color.terminfo} was copied from a previously built
# /run/current-system/sw/share/terminfo/r/rxvt-unicode-256color
{ runCommand }:

runCommand "rxvt-unicode-256color-terminfo" {} /* sh */ ''
  mkdir -p $out/nix-support
  mkdir -p $out/share/terminfo/r

  ln -s ${./rxvt-unicode-256color.terminfo} \
      $out/share/terminfo/r/rxvt-unicode-256color

  echo "$out" >> $out/nix-support/propagated-user-env-packages
''
