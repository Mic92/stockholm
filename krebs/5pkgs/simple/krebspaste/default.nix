{ bepasty-client-cli, gnused, writeDashBin }:

writeDashBin "krebspaste" ''
  ${bepasty-client-cli}/bin/bepasty-cli -L 1m --url http://paste.r "$@" |
  ${gnused}/bin/sed '
    $ {
      s/$/\/+inline/
      p
      s/\<r\>/krebsco.de/
    }
  '
''
