{ writeScriptBin, pkgs }:

# TODO: use `wrapProgram --add-flags` instead?
writeScriptBin "krebspaste" ''
  #! /bin/sh
  exec ${pkgs.bepasty-client-cli}/bin/bepasty-cli --url http://paste.retiolum "$@"
''
