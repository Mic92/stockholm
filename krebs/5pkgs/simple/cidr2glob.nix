{ python3, writeScriptBin, ... }:

let
  python = python3;
  pythonEnv = python.withPackages (ps: [ ps.netaddr ]);
in
  writeScriptBin "cidr2glob" ''
    #! ${pythonEnv}/bin/python

    import netaddr
    import re
    import sys

    def cidr2glob(cidr):
        net = netaddr.IPNetwork(cidr)

        if net.prefixlen <= 8:
            return map(lambda subnet: re.sub(r'\.0\.0\.0$', '.*', str(subnet.ip)), net.subnet(8))
        elif net.prefixlen <= 16:
            return map(lambda subnet: re.sub(r'\.0\.0$', '.*', str(subnet.ip)), net.subnet(16))
        elif net.prefixlen <= 24:
            return map(lambda subnet: re.sub(r'\.0$', '.*', str(subnet.ip)), net.subnet(24))
        else:
            return map(lambda ip: str(ip), list(net))

    if __name__ == "__main__":
        for cidr in sys.stdin:
            for glob in cidr2glob(cidr):
                print(glob)

  ''
