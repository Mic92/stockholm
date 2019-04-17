{ config, ... }:
{
  boot.initrd = {
    network = {
      enable = true;
      ssh = {
        enable = true;
        authorizedKeys = with config.krebs.users; [
          config.krebs.users.lass-mors.pubkey
          config.krebs.users.lass-blue.pubkey
          config.krebs.users.lass-shodan.pubkey
          config.krebs.users.lass-icarus.pubkey
        ];
      };
    };
  };
}
