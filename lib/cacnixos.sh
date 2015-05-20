. ./lib/net.sh

# cacnixos_networking : cac-server x hostname -> nixos-module
cacnixos_networking() {(
  server=$1
  hostname=$2

  address=$(echo $server | jq -r .ip)
  gateway=$(echo $server | jq -r .gateway)
  nameserver=8.8.8.8
  netmask=$(echo $server | jq -r .netmask)
  prefix=$(net_netmask_to_prefix $netmask)

  printf '{...}:\n'
  printf '{\n'
  printf '  networking.hostName = "%s";\n' $hostname
  printf '  networking.interfaces.enp2s1.ip4 = [\n'
  printf '    {\n'
  printf '      address = "%s";\n' $address
  printf '      prefixLength = %d;\n' $prefix
  printf '    }\n'
  printf '  ];\n'
  printf '  networking.defaultGateway = "%s";\n' $gateway
  printf '  networking.nameservers = [\n'
  printf '    "%s"\n' $nameserver
  printf '  ];\n'
  printf '}\n'
)}
