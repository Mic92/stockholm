{ curl, gnused, writeDashBin }:

writeDashBin "kpaste" ''
  ${curl}/bin/curl -sS http://p.r --data-binary @- |
  ${gnused}/bin/sed '$ {p;s/\<r\>/krebsco.de/}'
''
