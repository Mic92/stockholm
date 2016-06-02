{ pkgs, ... }:

# TODO use krebs.setuid
# This requires that we can create setuid executables that can only be accessed
# by a single user. [per-user-setuid]

# using bash for %q
pkgs.writeBashBin "ff" ''
 exec /var/setuid-wrappers/sudo -u ff -i <<EOF
 exec ${pkgs.firefoxWrapper}/bin/firefox $(printf " %q" "$@")
 EOF
''
