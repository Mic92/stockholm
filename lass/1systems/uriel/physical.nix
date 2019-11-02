{
  imports = [
    ./config.nix
  ];

  hardware.enableRedistributableFirmware = true;
  boot = {
    #kernelParams = [
    #  "acpi.brightness_switch_enabled=0"
    #];
    #loader.grub.enable = true;
    #loader.grub.version = 2;
    #loader.grub.device = "/dev/sda";

    loader.systemd-boot.enable = true;
    loader.timeout = 5;

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
    kernelModules = [ "msr" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/pool/root";
      fsType = "ext4";
    };

    "/bku" = {
      device = "/dev/pool/bku";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/sda1";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="64:27:37:7d:d8:ae", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:b8:c8:2e", NAME="et0"
  '';

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
    additionalOptions = ''
      Option "FingerHigh" "60"
      Option "FingerLow"  "60"
    '';
  };
}
