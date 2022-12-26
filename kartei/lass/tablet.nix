{ r6, w6, ... }:
{
  consul = false;
  nets = {
    wiregrill = {
      ip4.addr = "10.244.1.14";
      ip6.addr = w6 "b";
      aliases = [
        "tablet.w"
      ];
      wireguard.pubkey = "eIafsxYEFCqmWNFon6ZsYXeDrK4X1UJ9KD0zmNZjgEI=";
    };
  };
  external = true;
  ci = false;
}
