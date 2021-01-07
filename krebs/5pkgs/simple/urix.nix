let lib = import <stockholm/lib>; in
{ pkgs }:

# urix - URI eXtractor
# Extract all the URIs from standard input and write them to standard output!
# usage: urix < SOMEFILE

pkgs.execBin "urix" {
  filename = "${pkgs.gnugrep}/bin/grep";
  argv = [
    "urix"
    "-Eo"
    "\\b${lib.uri.posix-extended-regex}\\b"
  ];
}
