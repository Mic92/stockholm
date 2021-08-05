{ pkgs }:

let
  grep = "${pkgs.gnugrep}/bin/grep";
  xset = "${pkgs.xorg.xset}/bin/xset";
in

pkgs.writeDashBin "xtoggledpms" ''
  # usage: xtoggledpms
  set -efu
  if ${xset} q | ${grep} -qF 'DPMS is Disabled'; then
    ${xset} dpms force off
  else
    ${xset} s off -dpms
  fi
''
