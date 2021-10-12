{ pkgs, stockholm }:

# urix - URI eXtractor
# Extract all the URIs from standard input and write them to standard output!
# usage: urix < SOMEFILE

pkgs.execBin "urix" {
  filename = "${pkgs.gnugrep}/bin/grep";
  argv = [
    "urix"
    "-Eo"
    "\\b${stockholm.lib.uri.posix-extended-regex}\\b"
  ];
}
