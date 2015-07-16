{ pkgs, ... }:

with import ../../Zpkgs/tv { inherit pkgs; };

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
