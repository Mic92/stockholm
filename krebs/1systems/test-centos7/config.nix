{ config, lib, pkgs, ... }:

let
  inherit (lib) head;

in {
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/os-templates/CAC-CentOS-7-64bit.nix>
    <stockholm/krebs/2configs/temp/networking.nix>
    <stockholm/krebs/2configs/temp/dirs.nix>
  ];

  sound.enable = false;
  krebs.build.host = config.krebs.hosts.test-centos7;
}
