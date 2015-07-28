{ pkgs, ... }:

with import ../pkgs { inherit pkgs; };

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
