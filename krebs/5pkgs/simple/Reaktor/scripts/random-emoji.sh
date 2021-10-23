#!/bin/sh
curl -sS http://emojicons.com/random | \
  grep data-text | \
  sed -n 's/.*>\(.*\)<\/textarea>/\1/p' | \
  head -n 1 | \
  xmlstarlet unesc
