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
  ];

  krebs.build.host = config.krebs.hosts.blue;

  krebs.syncthing.folders = [
    { id = "contacts"; path = "/home/lass/contacts"; peers = [ "mors" "blue" "green" "phone" ]; }
    { path = "/home/lass/.weechat"; peers = [ "blue" "green" "mors" ]; }
  ];
  lass.ensure-permissions = [
    { folder = "/home/lass/contacts"; owner = "lass"; group = "syncthing"; }
    { folder = "/home/lass/.weechat"; owner = "lass"; group = "syncthing"; }
  ];

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
