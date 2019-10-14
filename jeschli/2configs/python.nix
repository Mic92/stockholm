{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    python37
    python37Packages.pip
    pipenv
  ];
}
