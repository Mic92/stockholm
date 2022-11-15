with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/mail.nix>

    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/sync.nix>
    <stockholm/lass/2configs/sync/decsync.nix>

    <stockholm/lass/2configs/weechat.nix>
    <stockholm/lass/2configs/bitlbee.nix>

    <stockholm/lass/2configs/muchsync.nix>
    <stockholm/lass/2configs/pass.nix>

    <stockholm/lass/2configs/git-brain.nix>
    <stockholm/lass/2configs/et-server.nix>
    <stockholm/lass/2configs/consul.nix>
  ];

  krebs.build.host = config.krebs.hosts.green;

  lass.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFlUMf943qEQG64ob81p6dgoHq4jUjq7tSvmSdEOEU2y";
  };

  systemd.tmpfiles.rules = [
    "d /var/state/lass_mail 0700 lass users -"
    "L+ /home/lass/Maildir - - - - ../../var/state/lass_mail"
    "d /home/lass/notmuch 0700 lass users -"
    "L+ /var/state/lass_mail/.notmuch - - - - /home/lass/notmuch"

    "d /var/state/lass_ssh 0700 lass users -"
    "L+ /home/lass/.ssh - - - - ../../var/state/lass_ssh"
    "d /var/state/lass_gpg 0700 lass users -"
    "L+ /home/lass/.gnupg - - - - ../../var/state/lass_gpg"
    "d /var/state/lass_sync 0700 lass users -"
    "L+ /home/lass/sync - - - - ../../var/state/lass_sync"

    "d /var/state/git 0700 git nogroup -"
    "L+ /var/lib/git - - - - ../../var/state/git"
  ];

  users.users.mainUser.openssh.authorizedKeys.keys = [
    config.krebs.users.lass-android.pubkey
    config.krebs.users.lass-tablet.pubkey
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKgpZwye6yavIs3gUIYvSi70spDa0apL2yHR0ASW74z8" # weechat ssh tunnel
  ];

  krebs.iptables.tables.nat.PREROUTING.rules = [
    { predicate = "-i eth0 -p tcp -m tcp --dport 22"; target = "ACCEPT"; precedence = 101; }
  ];

  # workaround for ssh access from yubikey via android
  services.openssh.extraConfig = ''
    HostKeyAlgorithms +ssh-rsa
    PubkeyAcceptedAlgorithms +ssh-rsa
  '';

  services.dovecot2 = {
    enable = true;
    mailLocation = "maildir:~/Maildir";
  };

  networking.firewall.allowedTCPPorts = [ 143 ];
}
