{ writeDashBin, bepasty-client-cli }:

# TODO use `execve` instead?
writeDashBin "krebspaste" ''
  exec ${bepasty-client-cli}/bin/bepasty-cli -L 1m --url http://paste.r "$@"
''
