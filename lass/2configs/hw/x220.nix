{ config, pkgs, ... }:
{
  imports = [
    <stockholm/krebs/2configs/hw/x220.nix>
  ];

  boot = {
    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda3"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    extraModulePackages = [
      config.boot.kernelPackages.tp_smapi
      config.boot.kernelPackages.acpi_call
    ];
    kernelModules = [
      "acpi_call"
      "tp_smapi"
    ];
  };

  environment.systemPackages = [
    pkgs.tpacpi-bat
  ];

  fileSystems = {
    "/" = {
      device = "/dev/mapper/pool-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/sda2";
    };
    "/home" = {
      device = "/dev/mapper/pool-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };

  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";

  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    START_CHARGE_THRESH_BAT0=80
    STOP_CHARGE_THRESH_BAT0=95
  '';

}
