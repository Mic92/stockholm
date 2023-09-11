{ pkgs, ... }:
pkgs.writers.writeDashBin "vicuna-chat" ''
  set -efu

  export PATH=${with pkgs; lib.makeBinPath [
    coreutils
    curl
    jq
  ]}

  CONTEXT=''${CONTEXT:-$(date -Id)}
  PROMPT=$*

  if ! test -e "$CONTEXT"; then
    echo -n 'null' > "$CONTEXT"
  fi

  add_to_context() {
    jq -rc --argjson message "$1" '. + [$message]' "$CONTEXT" > "$CONTEXT.tmp"
    mv "$CONTEXT.tmp" "$CONTEXT"
  }

  add_to_context "{\"role\": \"user\", \"content\": \"$PROMPT\"}"
  response=$(
    jq -nc --slurpfile context "$CONTEXT" '{
      model: "vicuna-13b-v1.5-16k",
      messages: $context[0],
    }' |
      curl -Ss http://vicuna.r/v1/chat/completions -H 'Content-Type: application/json' -d @-
  )
  add_to_context "$(jq -rcn --argjson response "$response" '$response.choices[0].message')"
  jq -rcn --argjson response "$response" '$response.choices[0].message.content'
''
