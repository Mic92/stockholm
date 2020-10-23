with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>

    <stockholm/lass/2configs/blue.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/sync/decsync.nix>
    <stockholm/lass/2configs/sync/weechat.nix>
  ];

  krebs.build.host = config.krebs.hosts.blue;

  networking.nameservers = [ "1.1.1.1" ];

  time.timeZone = "Europe/Berlin";
  users.users.mainUser.openssh.authorizedKeys.keys = [ config.krebs.users.lass-android.pubkey ];
}
