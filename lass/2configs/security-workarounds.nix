{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  # http://seclists.org/oss-sec/2017/q1/471
  boot.extraModprobeConfig = ''
    install dccp /run/current-system/sw/bin/false
  '';
}
