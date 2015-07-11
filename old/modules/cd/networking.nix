{...}:
{
  networking.hostName = "cd";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "162.219.7.216";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "162.219.7.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
