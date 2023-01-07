{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>

    # sync-containers
    <stockholm/lass/2configs/consul.nix>
    <stockholm/lass/2configs/yellow-host.nix>
    <stockholm/lass/2configs/radio/container-host.nix>

    # other containers
    <stockholm/lass/2configs/riot.nix>
  ];

  krebs.build.host = config.krebs.hosts.neoprism;
}
