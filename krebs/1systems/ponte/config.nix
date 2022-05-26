{ config, pkgs, ... }:
{
  imports = [
    ./hw.nix
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/matterbridge.nix>
  ];

  krebs.build.host = config.krebs.hosts.ponte;
}
