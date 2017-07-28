# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>

    <stockholm/krebs/2configs/buildbot-krebs.nix>
  ];

  krebs.build.host = config.krebs.hosts.hotdog;

  boot.isContainer = true;
  networking.useDHCP = false;
}
