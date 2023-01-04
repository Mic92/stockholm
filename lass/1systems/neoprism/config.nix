{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/consul.nix>
    <stockholm/lass/2configs/yellow-host.nix>
    <stockholm/lass/2configs/riot.nix>
  ];

  krebs.build.host = config.krebs.hosts.neoprism;
}
