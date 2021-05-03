{ config, pkgs, ... }:
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/tor-initrd.nix>
  ];

  krebs.build.host = config.krebs.hosts.echelon;

  boot.tmpOnTmpfs = true;
}

