{ config, ... }:
{
  users.users.root = {
    openssh.authorizedKeys.keys = [
      config.krebs.users.tv.pubkey
    ];
  };
}
