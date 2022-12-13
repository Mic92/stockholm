{ pkgs, lib, ... }:

pkgs.writers.writeBashBin "stable-generate" ''
  set -efu

  export PATH=${lib.makeBinPath [
    pkgs.coreutils
    pkgs.curl
    pkgs.jq
  ]}

  STABLE_URL=''${STABLE_URL:-http://stable-confusion.r}

  PAYLOAD=$(jq -cn --arg prompt "$*" '{
    prompt: $prompt
  }')

  filename=$(mktemp)
  curl -Ssf "$STABLE_URL/sdapi/v1/txt2img" \
    -X POST \
    --Header 'Content-Type: application/json' \
    --data "$PAYLOAD" |
      jq -r '.images[0]' |
      base64 --decode > "$filename"

  if test -t 1; then
    echo "$filename"
  else
    cat "$filename"
    rm "$filename"
  fi
''
