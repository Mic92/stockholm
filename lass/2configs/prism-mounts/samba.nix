{
  fileSystems."/mnt/prism" = {
    device = "//prism.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "x-systemd.automount"
      "x-systemd.device-timeout=1"
      "x-systemd.idle-timeout=1min"
    ];
  };
 }
