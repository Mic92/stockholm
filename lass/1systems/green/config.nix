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
    <stockholm/lass/2configs/sync/weechat.nix>

    <stockholm/lass/2configs/bitlbee.nix>
    <stockholm/lass/2configs/IM.nix>
    <stockholm/lass/2configs/muchsync.nix>
    <stockholm/lass/2configs/pass.nix>

    <stockholm/lass/2configs/git-brain.nix>
  ];

  krebs.build.host = config.krebs.hosts.green;

  users.users.mainUser.openssh.authorizedKeys.keys = [
    config.krebs.users.lass-android.pubkey
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK0rn3003CkJMk3jZrh/3MC6nVorHRymlFSI4x1brCKY" # weechat ssh tunnel
  ];

  krebs.bindfs = {
    "/home/lass/.weechat" = {
      source = "/var/state/lass_weechat";
      options = [
        "-M ${concatMapStringsSep ":" (u: toString config.users.users.${u}.uid) [ "syncthing" "mainUser" ]}"
        "--create-for-user=${toString config.users.users.syncthing.uid}"
      ];
    };
    "/home/lass/Maildir" = {
      source = "/var/state/lass_mail";
      options = [
        "-M ${toString config.users.users.mainUser.uid}"
      ];
    };
    "/home/lass/sync" = {
      source = "/var/state/lass_sync";
      options = [
        "-M ${concatMapStringsSep ":" (u: toString config.users.users.${u}.uid) [ "syncthing" "mainUser" ]}"
        "--create-for-user=${toString config.users.users.syncthing.uid}"
      ];
    };
    "/var/lib/bitlbee" = {
      source = "/var/state/bitlbee";
      options = [
        "-M ${toString config.users.users.bitlbee.uid}"
      ];
      clearTarget = true;
    };
    "/home/lass/.ssh" = {
      source = "/var/state/lass_ssh";
      options = [
        "-M ${toString config.users.users.mainUser.uid}"
      ];
      clearTarget = true;
    };
    "/home/lass/.gnupg" = {
      source = "/var/state/lass_gnupg";
      options = [
        "-M ${toString config.users.users.mainUser.uid}"
      ];
      clearTarget = true;
    };
    "/var/lib/git" = {
      source = "/var/state/git";
      options = [
        "-M ${toString config.users.users.git.uid}"
      ];
      clearTarget = true;
    };
  };

  systemd.services."bindfs-_home_lass_Maildir".serviceConfig.ExecStartPost = pkgs.writeDash "symlink-notmuch" ''
    sleep 1
    mkdir -p /home/lass/notmuch
    chown lass: /home/lass/notmuch
    ln -sfTr /home/lass/notmuch /home/lass/Maildir/.notmuch

    mkdir -p /home/lass/notmuch/muchsync
    chown lass: /home/lass/notmuch/muchsync
    mkdir -p /home/lass/Maildir/.muchsync
    ln -sfTr /home/lass/Maildir/.muchsync /home/lass/notmuch/muchsync/tmp
  '';

  krebs.iptables.tables.nat.PREROUTING.rules = [
    { predicate = "-i eth0 -p tcp -m tcp --dport 22"; target = "ACCEPT"; precedence = 101; }
  ];
}
