#! /bin/sh
set -euf

# require flock from util-linux (pkgs.util-linux)
if test "${FLOCK-}" != "$state_file"; then
  exec env FLOCK="$state_file" flock "$state_file" "$0" "$@"
fi

# TODO tell now, if already joined
jq -r <"$state_file" \
    --arg to "$_from" \
    --arg msgtarget "$_msgtarget" \
    '
      select(.to == $to and .msgtarget == $msgtarget) |
      "\(.to): \(.text) \u00032-- \(.from)\u00032 \(.date)"
    '

jq -c <"$state_file" >"$state_file.tmp" \
    --arg to "$_from" \
    --arg msgtarget "$_msgtarget" \
    '
      select((.to == $to and .msgtarget == $msgtarget) | not)
    '

mv "$state_file.tmp" "$state_file"
