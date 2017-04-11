{ curl, writeDashBin }:

writeDashBin "kpaste" ''
  exec ${curl}/bin/curl -sS http://p.r --data-binary @-
''
