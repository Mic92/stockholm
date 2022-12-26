{ r6, w6, ... }:
{
  consul = false;
  nets = {
    wiregrill = {
      ip4.addr = "10.244.1.13";
      ip6.addr = w6 "a";
      aliases = [
        "phone.w"
      ];
      wireguard.pubkey = "FY4PB8E/RC2JvtLgq/IDyMmZ9Ln6pz6eGyoytmUFMgk=";
    };
  };
  external = true;
  ci = false;
  syncthing.id = "PWKVXPB-JCNO6E4-KVIQ7CK-6FSOWHM-AWORMDU-HVVYLKW-44DQTYW-XZT7DQJ";
}
