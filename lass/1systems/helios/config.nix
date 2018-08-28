with import <stockholm/lib>;
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    dpass
  ];
}
