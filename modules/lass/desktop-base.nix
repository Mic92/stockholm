{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  time.timeZone = "Europe/Berlin";

  virtualisation.libvirtd.enable = true;

  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
  };

  programs.ssh.startAgent = false;

  security.setuidPrograms = [ "slock" ];

  services.printing = {
    enable = true;
    drivers = [ pkgs.foomatic_filters ];
  };

  environment.systemPackages = with pkgs; [

    powertop

  #window manager stuff
    haskellPackages.xmobar
    haskellPackages.yeganesh
    dmenu2
    xlibs.fontschumachermisc
  ];

}
