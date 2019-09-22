{ config, lib, ... }:
with import <stockholm/lib>;

{
  users.users.backup = {
    useDefaultShell = true;
    home = "/backups";
    createHome = true;
    group = "syncthing";
    openssh.authorizedKeys.keys = with config.krebs.hosts; [
      blue.ssh.pubkey
    ];
  };
}
