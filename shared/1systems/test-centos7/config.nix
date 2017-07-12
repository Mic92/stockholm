{ config, lib, pkgs, ... }:

let
  inherit (lib) head;

in {
  imports = [
    <stockholm/shared>
    <stockholm/shared/2configs/os-templates/CAC-CentOS-7-64bit.nix>
    <stockholm/shared/2configs/temp/networking.nix>
    <stockholm/shared/2configs/temp/dirs.nix>
  ];

  sound.enable = false;
  krebs.build.host = config.krebs.hosts.test-centos7;
}
