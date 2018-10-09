# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/nin>
    <stockholm/nin/2configs/retiolum.nix>
    <stockholm/nin/2configs/weechat.nix>
    <stockholm/nin/2configs/git.nix>
  ];

  krebs.build.host = config.krebs.hosts.onondaga;

  boot.isContainer = true;
  networking.useDHCP = false;

  time.timeZone = "Europe/Amsterdam";

  services.openssh.enable = true;
}
