{ config, pkgs, ... }:
{
  imports = [
    ./hw.nix
    <stockholm/krebs>
    <stockholm/krebs/2configs>
  ];

  krebs.build.host = config.krebs.hosts.ponte;
}
