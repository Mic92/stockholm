{
  imports = [
    ./config.nix
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/coreboot.nix>
  ];

  fileSystems = {
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:a0:0c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:71:cb:35", NAME="et0"
  '';

  services.thinkfan.enable = true;
}
