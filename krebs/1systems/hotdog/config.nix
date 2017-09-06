# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>

    <stockholm/krebs/2configs/buildbot-all.nix>
    <stockholm/krebs/2configs/gitlab-runner-shackspace.nix>
    <stockholm/krebs/2configs/binary-cache/nixos.nix>
  ];

  krebs.build.host = config.krebs.hosts.hotdog;

  boot.isContainer = true;
  networking.useDHCP = false;
  krebs.repo-sync.repos.stockholm.timerConfig = {
    OnBootSec = "5min";
    OnUnitInactiveSec = "2min";
    RandomizedDelaySec = "2min";
  };
  krebs.ci.stockholmSrc = "http://cgit.prism.r/stockholm";
}
