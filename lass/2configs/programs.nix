{ config, pkgs, ... }:

## TODO sort and split up
{
  environment.systemPackages = with pkgs; [
    aria2
    gnupg1compat
    htop
    i3lock
    mosh
    pass
    pavucontrol
    pv
    pwgen
    remmina
    ripgrep
    silver-searcher
    transmission
    wget
    xsel
    youtube-dl
    (pkgs.writeDashBin "tether-on" ''
      adb shell svc usb setFunctions rndis
    '')
    (pkgs.writeDashBin "tether-off" ''
      adb shell svc usb setFunctions
    '')
    (pkgs.writeDashBin "dl-movie" ''
      ${pkgs.transmission}/bin/transmission-remote yellow.r -w /var/download/finished/sorted/movies -a "$@"
    '')
    (pkgs.writeDashBin "dl-series" ''
      ${pkgs.transmission}/bin/transmission-remote yellow.r -w /var/download/finished/sorted/series -a "$@"
    '')
  ];
}
