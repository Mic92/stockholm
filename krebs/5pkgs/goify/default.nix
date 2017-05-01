{ pkgs, ... }:

pkgs.writeDashBin "goify" ''
  set -euf

  GO_HOST=''${GO_HOST:-go}

  while read line; do
    echo "$line" | sed -E 's|https?://\S*|\n&\n|g' | while read word; do
      if echo "$word" | grep -Eq ^https?:; then
        ${pkgs.curl}/bin/curl -Ss -F uri="$word" http://"$GO_HOST" | tr -d '\r'
      else
        echo "$word";
      fi;
    done | sed '/^$/d' | tr '\n' ' '; echo;
  done
''
