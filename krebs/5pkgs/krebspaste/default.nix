{ writeDashBin, bepasty-client-cli }:

# TODO use `execve` instead?
writeDashBin "krebspaste" ''
  exec ${bepasty-client-cli}/bin/bepasty-cli --url http://paste.r "$@"
''
