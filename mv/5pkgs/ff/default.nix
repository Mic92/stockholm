{ pkgs, ... }:

pkgs.writeScriptBin "ff" ''
 #! ${pkgs.bash}/bin/bash
 exec sudo -u ff -i <<EOF
 exec ${pkgs.firefoxWrapper}/bin/firefox $(printf " %q" "$@")
 EOF
''
