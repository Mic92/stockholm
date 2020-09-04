{
  boot.initrd.luks.devices.main.device = "/dev/sda2";
  fileSystems."/" = {
    device = "/dev/main/root";
    options = ["defaults" "noatime" "commit=60"];
  };
  fileSystems."/boot" = {
    device = "/dev/sda1";
    options = ["defaults" "noatime"];
  };
  fileSystems."/bku" = {
    device = "/dev/main/bku";
    options = ["defaults" "noatime"];
  };
  fileSystems."/home" = {
    device = "/dev/main/home";
    options = ["defaults" "noatime" "commit=60"];
  };
}
