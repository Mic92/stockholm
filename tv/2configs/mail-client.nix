{ lib, pkgs, ... }:

with import ../5pkgs { inherit lib pkgs; };

{
  environment.systemPackages = [
    much
    msmtp
    notmuch
    pythonPackages.alot
    qprint
    w3m
  ];
}
