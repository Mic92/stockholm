{ dmenu, gnused, pass, writeDashBin, xdotool }:

writeDashBin "otpmenu" ''
  set -efu

  x=$(
    ${pass}/bin/pass git ls-files '*/otp.gpg' \
      | ${gnused}/bin/sed 's:/otp\.gpg$::' \
      | ${dmenu}/bin/dmenu -f -p OTP
  )

  otp=$(${pass}/bin/pass otp code "$x/otp")

  printf %s "$otp" | ${xdotool}/bin/xdotool type -f -
''
