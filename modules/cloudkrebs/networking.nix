{...}:
{
  networking.hostName = "cloudkrebs";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "104.167.113.104";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "104.167.113.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
