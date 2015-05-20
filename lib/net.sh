net_netmask_to_prefix() {(
  binaryNetmask=$(echo $1 | sed 's/^/obase=2;/;s/\./;/g' | bc | tr -d \\n)
  binaryPrefix=$(echo $binaryNetmask | sed -n 's/^\(1*\)0*$/\1/p')
  if ! echo $binaryPrefix | grep -q .; then
    echo $0: bad netmask: $netmask >&2
    exit 4
  fi
  printf %s $binaryPrefix | tr -d 0 | wc -c
)}
