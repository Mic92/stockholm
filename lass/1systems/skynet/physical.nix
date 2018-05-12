{
  imports = [
    ./config.nix
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="10:0b:a9:a6:44:04", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:d1:90:fc", NAME="et0"
  '';
}
