{ config, pkgs, ... }:

{
  imports = [
    ../.
  ];
  programs.ssh.startAgent = true;
  programs.ssh.startAgent = false;
}
