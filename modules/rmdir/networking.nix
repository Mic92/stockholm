_:

{
  networking.hostName = "rmdir";
  networking.interfaces.enp2s1.ip4 = [
    {
      address = "167.88.44.94";
      prefixLength = 24;
    }
  ];
  networking.defaultGateway = "167.88.44.1";
  networking.nameservers = [
    "8.8.8.8"
  ];
}
