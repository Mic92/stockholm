{ config, pkgs, ... }:

{

  imports = [
    ./games.nix
  ];
  #
  # Steam stuff
  # source: https://nixos.org/wiki/Talk:Steam
  #
  ##TODO: make steam module
  hardware.opengl.driSupport32Bit = true;

  environment.systemPackages = with pkgs; [
    steam
  ];
  networking.firewall = {
    allowedUDPPorts = [
      27031
      27036
    ];
    allowedTCPPorts = [
      27036
      27037
    ];
  };

}
