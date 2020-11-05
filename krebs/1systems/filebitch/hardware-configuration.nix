{ config, lib, pkgs, ... }:
let
  byid = dev: "/dev/disk/by-id/" + dev;
  keyFile = byid "usb-SMI_USB_DISK_AA08061700009650-0:0";
in
{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.zfs.devNodes = "/dev"; # fixes some virtualmachine issues
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;
  boot.kernelParams = [
    "boot.shell_on_fail"
    "panic=30" "boot.panic_on_fail" # reboot the machine upon fatal boot issues
  ];
  boot.tmpOnTmpfs = true;


  boot.initrd.availableKernelModules = [
    "xhci_pci" "ahci" "ohci_pci" "ehci_pci" "usb_storage" "usbhid" "sd_mod"
    "raid456"
    "usbhid"
    "usb_storage"
  ];
  boot.initrd.kernelModules = [
    "sata_sil"
    "megaraid_sas"
  ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "tank/root";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "tank/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "tank/nix";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/5266-931D";
      fsType = "vfat";
    };
  fileSystems."/serve" =
    { device = "/dev/cryptvg/serve";
      fsType = "ext4";
      options = [ "nofail" ];
    };
  fileSystems."/serve/incoming" =
    { device = "/dev/cryptvg/incoming";
      fsType = "ext4";
      options = [ "nofail" ];

    };
  fileSystems."/serve/movies" =
    { device = "/dev/cryptvg/servemovies";
      fsType = "ext4";
      options = [ "nofail" ];
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/3353c76f-50e4-471d-84bc-ff922d22b271"; }
    ];

    nix.maxJobs = lib.mkDefault 4;
  boot.loader.grub.device = byid "ata-INTEL_SSDSA2M080G2GC_CVPO013300WD080BGN";

  networking.hostId = "54d97450"; # required for zfs use
  boot.initrd.luks.devices = let
        usbkey = device: {
          inherit device keyFile;
          keyFileSize = 2048;
          preLVM = true;
        };
  in {
    swap = ((usbkey (byid "ata-INTEL_SSDSA2M080G2GC_CVPO013300WD080BGN-part2"))
      // { allowDiscards = true; } );
    root = ((usbkey (byid "ata-INTEL_SSDSA2M080G2GC_CVPO013300WD080BGN-part3"))
      // { allowDiscards = true; } );
    md125 = usbkey "/dev/md125";
    md126 = usbkey "/dev/md126";
    md127 = usbkey "/dev/md127";
  };
}
