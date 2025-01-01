{ config, ... }:
{
  users.domi = {
    mail = "mail@domi.de";
  };
  hosts.domi = {
    owner = config.krebs.users.domi;
    nets.wiregrill = {
      ip4.addr = "10.244.10.108";
      aliases = [
        "domi.w"
      ];
      wireguard.pubkey = "Yy1pvM0lEwaXuOwBoFGNYhHeyYEKuR/rovE0/myWyCI=";
    };
  };
}
