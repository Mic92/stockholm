#! /bin/sh
set -eu
printf "Sie meinten wohl \""
echo -n $@ | sed 's/Shack/shack/g'
echo "\" check out https://wiki.shackspace.de/project/logo_and_ci#name_ci"
echo "${_from}--"
