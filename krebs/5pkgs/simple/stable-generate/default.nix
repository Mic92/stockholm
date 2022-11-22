{ pkgs, lib, ... }:

pkgs.writers.writeDashBin "stable-generate" ''
  set -efu

  export PATH=${lib.makeBinPath [
    pkgs.curl
    pkgs.jq
  ]}

  STABLE_URL=''${STABLE_URL:-http://stable-confusion.r}

  PAYLOAD=$(jq -cn --arg query "$*" '{fn_index: 51, data: [
    $query,
    "",
    "None",
    "None",
    20, # sampling steps
    "Euler a", # sampling method
    false, # restore faces
    false,
    1,
    1,
    7,
    -1,
    -1,
    0,
    0,
    0,
    false,
    512, #probably resolution
    512, #probably resolution
    false,
    0.7,
    0,
    0,
    "None",
    "",
    false,
    false,
    false,
    "",
    "Seed",
    "",
    "Nothing",
    "",
    true,
    false,
    false,
    null,
    "",
  ""], session_hash: "hello_this_is_dog"}')

  data=$(curl -Ssf "$STABLE_URL/run/predict/" \
    -X POST \
    --Header 'Content-Type: application/json' \
    --data "$PAYLOAD"
  )
  export data

  filename=$(jq -rn 'env.data | fromjson.data[0][0].name')

  echo "$STABLE_URL/file=$filename"
''
