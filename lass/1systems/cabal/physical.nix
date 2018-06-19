{
  imports = [
    ./config.nix
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:45:85:ac", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:62:2b:1b", NAME="et0"
  '';
}
