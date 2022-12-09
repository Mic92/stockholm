with import ./lib;
{ config, pkgs, ... }: {
  krebs.build.host = config.krebs.hosts.zu;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/gitrepos.nix>
    <stockholm/tv/2configs/mail-client.nix>
    <stockholm/tv/2configs/man.nix>
    <stockholm/tv/2configs/nginx/public_html.nix>
    <stockholm/tv/2configs/pulse.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xserver>
  ];

  boot.initrd.luks.devices.zuca.device = "/dev/sda2";

  fileSystems = {
    "/" = {
      device = "/dev/mapper/zuvga-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/bku" = {
      device = "/dev/mapper/zuvga-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/zuvga-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/sda1";
    };
  };

  networking.wireless.enable = true;

  services.printing.enable = true;

  #services.bitlbee.enable = true;
  #services.tor.client.enable = true;
  #services.tor.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
