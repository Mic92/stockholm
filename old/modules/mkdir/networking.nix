{...}:
{
  networking.hostName = "mkdir";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "162.248.167.241";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "162.248.167.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
