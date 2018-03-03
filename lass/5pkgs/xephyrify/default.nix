{ writeDashBin, coreutils, xorg, virtualgl, ... }:

writeDashBin "xephyrify" ''
  NDISPLAY=:$(${coreutils}/bin/shuf -i 100-65536 -n 1)
  ${xorg.xorgserver}/bin/Xephyr -br -ac -reset -terminate -resizeable $NDISPLAY &
  XEPHYR_PID=$!
  DISPLAY=$NDISPLAY ${virtualgl}/bin/vglrun "$@"
  kill $XEPHYR_PID
''
