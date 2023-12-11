{ config, lib, pkgs, ... }:

with import ../../lib/pure.nix { inherit lib; };
{
  imports = [
    ./backup.nix
    ./security-workarounds.nix
  ];
  krebs.announce-activation.enable = true;
  krebs.enable = true;
  krebs.tinc.retiolum.enable = mkDefault true;

  # trust krebs ACME CA
  krebs.ssl.trustIntermediate = true;

  krebs.build.user = mkDefault config.krebs.users.krebs;

  networking.hostName = config.krebs.build.host.name;

  nix.maxJobs = 1;
  nix.useSandbox = true;

  environment.systemPackages = with pkgs; [
    git
    vim
    rxvt_unicode.terminfo
  ];

  console.keyMap = "us";

  programs.ssh.startAgent = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      { type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };
  services.cron.enable = false;
  services.ntp.enable = false;

  # limit journald size
  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
    Storage=persistent
  '';

  users.mutableUsers = false;
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    config.krebs.users.lass.pubkey
    config.krebs.users.makefu.pubkey
    config.krebs.users.tv.pubkey
    config.krebs.users.kmein.pubkey
    config.krebs.users.mic92.pubkey
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  # maybe fix Error: unsupported locales detected:
  i18n.defaultLocale = mkDefault "C.UTF-8";
}
