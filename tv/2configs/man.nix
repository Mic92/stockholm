{ config, lib, pkgs, ... }:
{
  #environment.etc."man.conf".source = pkgs.runCommand "man.conf" {} ''
  #  ${pkgs.gnused}/bin/sed <${pkgs.man}/lib/man.conf >$out '
  #    s:^NROFF\t.*:& -Wbreak:
  #  '
  #'';
  environment.systemPackages = with pkgs; [
    manpages
    posix_man_pages
  ];
}
