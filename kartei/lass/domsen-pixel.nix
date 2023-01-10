{ r6, w6, ... }:
{
  consul = false;
  nets = {
    wiregrill = {
      ip4.addr = "10.244.1.17";
      ip6.addr = w6 "d0";
      aliases = [
        "domsen-pixel.w"
      ];
      wireguard.pubkey = "cGuBSB1DftIsanbxrSG/i4FiC+TmQrs+Z0uE6SPscHY=";
    };
  };
  external = true;
  ci = false;
}
