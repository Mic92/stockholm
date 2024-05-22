let
  ext-if = "et0";
  shack-ip = "10.42.22.184";
  shack-gw = "10.42.20.1";
in {
  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="8c:70:5a:b2:84:58", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="3c:97:0e:07:b9:14", NAME="${ext-if}"
  '';
  networking.wireless.enable = true;
  networking = {
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 80 443 8088 8086 8083 5901 ];
    interfaces."${ext-if}".ipv4.addresses = [
      {
        address = shack-ip;
        prefixLength = 20;
      }
    ];

    defaultGateway = shack-gw;
    nameservers = [ "10.42.0.100" "10.42.0.200" ];
  };
}
