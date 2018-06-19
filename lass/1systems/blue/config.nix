with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>

    <stockholm/lass/2configs/blue.nix>
  ];

  krebs.build.host = config.krebs.hosts.blue;

  environment.shellAliases = {
    deploy = pkgs.writeDash "deploy" ''
      set -eu
      export SYSTEM="$1"
      $(nix-build $HOME/stockholm/lass/krops.nix --no-out-link --argstr name "$SYSTEM" -A deploy)
    '';
  };

  networking.nameservers = [ "1.1.1.1" ];

  lass.restic = genAttrs [
    "daedalus"
    "icarus"
    "littleT"
    "prism"
    "shodan"
    "skynet"
  ] (dest: {
    dirs = [
      "/home/"
      "/var/lib"
    ];
    passwordFile = (toString <secrets>) + "/restic/${dest}";
    repo = "sftp:backup@${dest}.r:/backups/blue";
    extraArguments = [
      "sftp.command='ssh backup@${dest}.r -i ${config.krebs.build.host.ssh.privkey.path} -s sftp'"
    ];
    timerConfig = {
      OnCalendar = "00:05";
      RandomizedDelaySec = "5h";
    };
  });
  time.timeZone = "Europe/Berlin";
  users.users.mainUser.openssh.authorizedKeys.keys = [ config.krebs.users.lass-android.pubkey ];
}
