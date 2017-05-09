{ pkgs, ... }:

pkgs.writeBashBin "goify" ''
  set -euf

  GO_HOST=''${GO_HOST:-go}

  while read line; do
    echo "$line" | sed 's|https\?://\S*|\n&\n|g' | while read word; do
      if echo "$word" | grep -q '^https\?:'; then
        ${pkgs.curl}/bin/curl -Ss -F uri="$word" http://"$GO_HOST" \
          | tr -d '\r'
      else
        echo "$word"
      fi
    done | grep . | tr '\n' ' '; echo
  done
''
