{ config, lib, pkgs, ... }:

let
  inherit (lib) head;

in {
  imports = [
    ../.
    ../2configs/os-templates/CAC-CentOS-7-64bit.nix
    ../2configs/temp/networking.nix
    ../2configs/temp/dirs.nix
  ];

  sound.enable = false;
  krebs.build.host = config.krebs.hosts.test-centos7;
}
