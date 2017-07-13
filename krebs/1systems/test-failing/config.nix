{ config, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
  ];
  programs.ssh.startAgent = true;
  programs.ssh.startAgent = false;
}
