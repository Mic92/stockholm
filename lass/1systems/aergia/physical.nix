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

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.kernelParams = [
    # Enable energy savings during sleep
    "mem_sleep_default=deep"

    "amd_pstate=passive"

    # for ryzenadj -i
    "iomem=relaxed"
  ];

  boot.kernelModules = [
    # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html
    # On recent AMD CPUs this can be more energy efficient.
    "amd-pstate"
    "kvm-amd"

    # needed for zenstates
    "msr"

    # zenpower
    "zenpower"
  ];

  boot.extraModulePackages = [
    (config.boot.kernelPackages.zenpower.overrideAttrs (old: {
      src = pkgs.fetchFromGitea {
        domain = "git.exozy.me";
        owner = "a";
        repo = "zenpower3";
        rev = "c176fdb0d5bcba6ba2aba99ea36812e40f47751f";
        hash = "sha256-d2WH8Zv7F0phZmEKcDiaak9On+Mo9bAFhMulT/N5FWI=";
      };
    }))
  ];

  # hardware.cpu.amd.updateMicrocode = true;

  services.xserver.videoDrivers = [
    "amdgpu"
  ];

  boot.initrd.availableKernelModules = [
    "nvme"
    "thunderbolt"
    "xhci_pci"
    "usbhid"
  ];

  boot.initrd.kernelModules = [
    "amdgpu"
  ];

  environment.systemPackages = [
    pkgs.vulkan-tools
    pkgs.ryzenadj
    (pkgs.writers.writeDashBin "set_tdp" ''
      set -efux
      watt=$1
      value=$(( $watt * 1000 ))
      ${pkgs.ryzenadj}/bin/ryzenadj --stapm-limit="$value" --fast-limit="$value" --slow-limit="$value"
    '')
  ];

  # textsize
  services.xserver.dpi = 200;
  hardware.video.hidpi.enable = lib.mkDefault true;

  # corectrl
  programs.corectrl = {
    enable = true;
    gpuOverclock = {
      enable = true;
      ppfeaturemask = "0xffffffff";
    };
  };
  users.users.mainUser.extraGroups = [ "corectrl" ];

  # use newer ryzenadj
  nixpkgs.config.packageOverrides = super: {
    ryzenadj = super.ryzenadj.overrideAttrs (old: {
      version = "unstable-2023-01-15";
      src = pkgs.fetchFromGitHub {
        owner = "FlyGoat";
        repo = "RyzenAdj";
        rev = "1052fb52b2c0e23ac4cd868c4e74d4a9510be57c"; # unstable on 2023-01-15
        sha256 = "sha256-/IxkbQ1XrBrBVrsR4EdV6cbrFr1m+lGwz+rYBqxYG1k=";
      };
    });
  };

  # keyboard quirks
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e 'keycode 96 = F12 Insert F12 F12' # rebind shift + F12 to shift + insert
  '';
  services.udev.extraHwdb = /* sh */ ''
    # disable back buttons
    evdev:input:b0003v2F24p0135* # /dev/input/event2
      KEYBOARD_KEY_70026=reserved
      KEYBOARD_KEY_70027=reserved
  '';

  # ignore power key
  services.logind.extraConfig = "HandlePowerKey=ignore";

  # update cpu microcode
  hardware.cpu.amd.updateMicrocode = true;
}
