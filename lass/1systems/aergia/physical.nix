{ config, lib, pkgs, modulesPath, ... }:
{
  imports = [
    ./config.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
  disko.devices = import ./disk.nix;

  networking.hostId = "deadbeef";
  # boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable = true;
    device = "/dev/nvme0n1";
    efiSupport = true;
    efiInstallAsRemovable = true;
  };


  # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html
  # On recent AMD CPUs this can be more energy efficient.
  boot.kernelModules = [ "kvm-amd" ];

  # hardware.cpu.amd.updateMicrocode = true;

  services.xserver.videoDrivers = [
    "amdgpu"
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "usbhid" "usb_storage" "sd_mod" ];

  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";

  environment.systemPackages = [
    pkgs.ryzenadj
  ];

  # textsize
  services.xserver.dpi = 200;
}
