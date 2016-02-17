{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    manpages
    posix_man_pages
  ];
}
