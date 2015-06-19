{...}:
{
  networking.hostName = "rmdir";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "162.219.6.2";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "162.219.6.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
