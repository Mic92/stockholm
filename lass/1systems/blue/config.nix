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

  environment.shellAliases = {
    deploy = pkgs.writeDash "deploy" ''
      set -eu
      export SYSTEM="$1"
      $(nix-build $HOME/sync/stockholm/lass/krops.nix --no-out-link --argstr name "$SYSTEM" -A deploy)
    '';
  };

  networking.nameservers = [ "1.1.1.1" ];

  services.restic.backups = genAttrs [
    "daedalus"
    "icarus"
    "littleT"
    "prism"
    "shodan"
    "skynet"
  ] (dest: {
    initialize = true;
    extraOptions = [
      "sftp.command='ssh backup@${dest}.r -i ${config.krebs.build.host.ssh.privkey.path} -s sftp'"
    ];
    repository = "sftp:backup@${dest}.r:/backups/blue";
    passwordFile = (toString <secrets>) + "/restic/${dest}";
    timerConfig = { OnCalendar = "00:05"; RandomizedDelaySec = "5h"; };
    paths = [
      "/home/"
      "/var/lib"
    ];
  });

  time.timeZone = "Europe/Berlin";
  users.users.mainUser.openssh.authorizedKeys.keys = [ config.krebs.users.lass-android.pubkey ];
}
