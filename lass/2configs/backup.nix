{ config, lib, ... }:
with import <stockholm/lib>;

{
  fileSystems = {
    "/backups" = {
      device = "/dev/pool/backup";
      fsType = "ext4";
    };
  };
  users.users.backup = {
    useDefaultShell = true;
    home = "/backups";
    createHome = true;
    openssh.authorizedKeys.keys = with config.krebs.hosts; [
      mors.ssh.pubkey
      prism.ssh.pubkey
      blue.ssh.pubkey
    ];
  };
}
