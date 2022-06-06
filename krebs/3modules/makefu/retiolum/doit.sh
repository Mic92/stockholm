#!/bin/sh


for i in *.pub;do
  sleep 1
  host=${i%%.pub}
  if test -e "${host}_ed25519.pub" ;then
    echo "ed25519 key already exits"
  else
    tmpdir=$(mktemp --tmpdir -d create-retiolum-ed25519-keys.XXXXXXXX)
    trap 'rm -vR "$tmpdir"' EXIT
    tinc --batch --config "$tmpdir" generate-ed25519-keys

    cp -v "$tmpdir/ed25519_key.pub" "${host}_ed25519.pub"
    cat "$tmpdir/ed25519_key.priv" | secrets insert -m "$host/retiolum.ed25519_key.priv"
    rm -vR "$tmpdir"
  fi

done
