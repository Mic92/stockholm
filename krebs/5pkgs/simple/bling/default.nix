{ imagemagick, runCommand, ... }:

with import <stockholm/lib>;

let
  krebs-v2 = [
    "                "
    "                "
    " x x         x x"
    "xx x  xx xx xx x"
    "xx x  xx xx xx x"
    " xxx   x  x  xxx"
    " xxx  xxxxx  xxx"
    "  x  xxxxxxx  x "
    "  xxxxxxxxxxxxx "
    "     xxxxxxx    "
    "   xxxxxxxxxxx  "
    "   x   xxx   x  "
    "  x  x x x x  x "
    "  x  x x x x  x "
    "  x xx x x xx x "
    "                "
  ];

  chars-per-pixel = 1;
  colors = 2;
  columns = foldl' max 0 (map stringLength krebs-v2);
  rows = length krebs-v2;

  png-geometry = "1692x1692";

  txt = concatMapStrings (s: "${s}\n") krebs-v2;

  xpm = ''
    static char *krebs_v2[] = {
      ${toC (toString [columns rows colors chars-per-pixel])},
      "  c None",
      "x c #E4002B",
      ${concatMapStringsSep ",\n  " toC krebs-v2}
    };
  '';
in

runCommand "bling"
  {
    inherit xpm;
    passAsFile = ["xpm"];
  }
  ''
    mkdir -p $out
    cd $out

    cp $xpmPath krebs-v2.xpm
    ${imagemagick}/bin/convert krebs-v2.xpm krebs-v2.ico
    ${imagemagick}/bin/convert krebs-v2.xpm -scale ${png-geometry} krebs-v2.png
  ''
