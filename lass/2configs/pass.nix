{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pass
    gnupg1
  ];

}
