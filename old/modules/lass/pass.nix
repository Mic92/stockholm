{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pass
    gnupg1
  ];

  services.xserver.startGnuPGAgent = true;
}
