{ writeScriptBin, pkgs }:

# TODO: add krebs CA to toolchain, remove --insecure
# TODO: use `wrapProgram --add-flags` instead?

writeScriptBin "krebspaste" ''
  #! /bin/sh
  exec ${pkgs.bepasty-client-cli}/bin/bepasty-cli --insecure --url http://paste.retiolum "$@"
''
