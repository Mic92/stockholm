{ config, pkgs, ... }:

{
  imports = [
    <stockholm/shared>
  ];
  programs.ssh.startAgent = true;
  programs.ssh.startAgent = false;
}
