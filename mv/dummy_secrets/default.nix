{ config, ... }:
{
  users.users.root = {
    openssh.authorizedKeys.keys = [
      config.krebs.users.mv.pubkey
    ];
  };
}
