with import ./lib;
{ config, pkgs, ... }: {
  imports = [
    ../smartd.nix
    {
      boot.extraModulePackages = [
        config.boot.kernelPackages.acpi_call
      ];

      boot.kernelModules = [
        "acpi_call"
      ];

      environment.systemPackages = [
        pkgs.tpacpi-bat
      ];
    }

    # fix jumpy touchpad
    # https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X220#X220_Touchpad_cursor_jump/imprecise
    {
      services.udev.extraHwdb = /* sh */ ''
        touchpad:i8042:*
         LIBINPUT_MODEL_LENOVO_X220_TOUCHPAD_FW81=1
      '';
    }

    {
      nix.settings.cores = 2;
      nix.settings.max-jobs = 2;
    }
    (if lib.versionAtLeast (lib.versions.majorMinor lib.version) "21.11" then {
      nix.daemonCPUSchedPolicy = "batch";
      nix.daemonIOSchedPriority = 1;
    } else {
      nix.daemonIONiceLevel = 1;
      nix.daemonNiceLevel = 1;
    })
  ];

  boot.extraModulePackages = [
    config.boot.kernelPackages.tp_smapi
  ];

  boot.kernelModules = [ "tp_smapi" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Required for Centrino.
  hardware.enableRedistributableFirmware = true;

  hardware.opengl.extraPackages = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  # Conflicts with TLP, but gets enabled by DEs.
  services.power-profiles-daemon.enable = false;

  services.tlp.enable = true;
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 80;
  };


  services.logind.extraConfig = ''
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
  '';

  # because extraConfig is not extra enough:
  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";

  services.xserver = {
    videoDriver = "intel";
  };

  tv.hw.screens.primary.width = lib.mkDefault 1366;
  tv.hw.screens.primary.height = lib.mkDefault 768;
}
