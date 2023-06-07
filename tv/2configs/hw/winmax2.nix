{ pkgs, ... }: {

  imports = [
    ../smartd.nix
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "thunderbolt" "usbhid" ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.kernelModules = [
    "amd-pstate"
    "kvm-amd"
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "amd_pstate=passive"
  ];

  hardware.bluetooth.enable = true;

  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = [
    pkgs.amdvlk
    pkgs.rocm-opencl-icd
    pkgs.rocm-opencl-runtime
  ];

  networking.wireless.enable = true;
  networking.wireless.interfaces = [
    "wlp1s0"
  ];
  networking.interfaces.wlp1s0.useDHCP = true;

  nixpkgs.hostPlatform = "x86_64-linux";

  services.illum.enable = true;

  services.logind.extraConfig = /* ini */ ''
    HandlePowerKey=ignore
  '';

  tv.lidControl.enable = true;

  tv.hw.screens.primary.width = 2560;
  tv.hw.screens.primary.height = 1600;
}
