# hostname: mqtt.shack
{
  networking.firewall.allowedTCPPorts = [ 1883 ];
  networking.firewall.allowedUDPPorts = [ 1883 ];
  services.mosquitto = {
    enable = true;
    host = "0.0.0.0";
    users = {};
    # TODO: secure that shit
    aclExtraConf = ''
      pattern readwrite #
    '';
    allowAnonymous = true;
  };
}
