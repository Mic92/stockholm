with import <stockholm/lib>;
{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/otp-ssh.nix>
    <stockholm/lass/2configs/git.nix>
    { # automatic hardware detection
      boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" =
        { device = "/dev/pool/root";
          fsType = "btrfs";
        };

      fileSystems."/boot" =
        { device = "/dev/disk/by-uuid/1F60-17C6";
          fsType = "vfat";
        };

      fileSystems."/home" =
        { device = "/dev/pool/home";
          fsType = "btrfs";
        };

      nix.maxJobs = lib.mkDefault 8;
      powerManagement.cpuFreqGovernor = "powersave";
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
    {
      services.xserver.dpi = 200;
      fonts.fontconfig.dpi = 200;
      lass.myFont = "-schumacher-clean-*-*-*-*-25-*-*-*-*-*-iso10646-1";
    }
  ];
  krebs.build.host = config.krebs.hosts.helios;

  krebs.git.rules = [
    {
      user = [ config.krebs.users.lass-helios ];
      repo = [ config.krebs.git.repos.stockholm ];
      perm = with git; push "refs/heads/*" [ fast-forward non-fast-forward create delete merge ];
    }
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.wireless.enable = true;
  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    vim
    rxvt_unicode
    git
    rsync
    hashPassword
    thunderbird
    dpass
  ];

  users.users = {
    root.openssh.authorizedKeys.keys = [
      config.krebs.users.lass-helios.pubkey
    ];
  };

  programs.ssh.startAgent = lib.mkForce true;

}
