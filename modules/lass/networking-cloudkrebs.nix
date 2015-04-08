{...}:
{
  networking.hostName = "cloudkrebs";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "104.167.112.39";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "104.167.112.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
