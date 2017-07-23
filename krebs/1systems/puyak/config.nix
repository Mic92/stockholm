{ config, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/secret-passwords.nix>
    <stockholm/krebs/2configs/hw/x220.nix>

    <stockholm/krebs/2configs/repo-sync.nix>
    <stockholm/krebs/2configs/shared-buildbot.nix>
    <stockholm/krebs/2configs/stats/puyak-client.nix>
  ];

  krebs.build.host = config.krebs.hosts.puyak;

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda3"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/pool-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/sda2";
    };
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
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

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="8c:70:5a:b2:84:58", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="3c:97:0e:07:b9:14", NAME="et0"
  '';

}
