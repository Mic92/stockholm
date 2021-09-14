#! /bin/sh
set -euf

# require flock from util-linux
if test "${FLOCK-}" != "$state_file"; then
  exec env FLOCK="$state_file" flock "$state_file" "$0" "$@"
fi

# TODO tell now, if already joined
jq -cn \
    --arg from "$_from" \
    --arg to "${1%% *}" \
    --arg text "${1#* }" \
    --arg msgtarget "$_msgtarget" \
    '{ $from, $to, $text, $msgtarget, date: (now | todate) }' \
  >> "$state_file"

echo 'Consider it noted.' # that's what lambdabot says...
