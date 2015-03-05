{ config, pkgs, ... }:

{
  time.timeZone = "Europe/Berlin";

  nix.maxJobs = 8;
  nix.useChroot = true;
  # TODO check if both are required:
  nix.chrootDirs = [ "/etc/protocols" pkgs.iana_etc.outPath ];

  nix.trustedBinaryCaches = [
    "https://cache.nixos.org"
    "http://cache.nixos.org"
    "http://hydra.nixos.org"
  ];

}
