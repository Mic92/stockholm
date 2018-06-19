{ pkgs, ... }:
{
  imports = [
    ./config.nix
    { # automatic hardware detection
      boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" = {
        device = "/dev/pool/root";
        fsType = "btrfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/1F60-17C6";
        fsType = "vfat";
      };

      fileSystems."/home" = {
        device = "/dev/pool/home";
        fsType = "btrfs";
      };

      fileSystems."/tmp" = {
        device = "tmpfs";
        fsType = "tmpfs";
        options = ["nosuid" "nodev" "noatime"];
      };
    }
    { # crypto stuff
      boot.initrd.luks = {
        cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
        devices =  [{
           name = "luksroot";
           device = "/dev/nvme0n1p3";
        }];
      };
    }
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.wireless.enable = true;
  hardware.enableRedistributableFirmware = true;


  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="f8:59:71:a9:05:65", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="54:e1:ad:4f:06:83", NAME="et0"
  '';

  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.xrandrHeads = [
    { output = "DP-2"; primary = true; }
    { output = "DP-4"; monitorConfig = ''Option "Rotate" "left"''; }
    { output = "DP-0"; }
  ];

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output DP-6 --off --output DP-5 --off --output DP-4 --mode 2560x1440 --pos 3840x0 --rotate left --output DP-3 --off --output DP-2 --primary --mode 3840x2160 --scale 0.5x0.5 --pos 0x400 --rotate normal --output DP-1 --off --output DP-0 --mode 2560x1440 --pos 5280x1120 --rotate normal
  '';
}
