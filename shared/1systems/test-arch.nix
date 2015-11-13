{ config, pkgs, ... }:

{
  imports = [
    ../2configs/base.nix
    {
      boot.loader.grub = {
        device = "/dev/sda";
        splashImage = null;
      };

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "vmw_pvscsi"
      ];

      fileSystems."/" = {
        device = "/dev/sda1";
      };
    }
    {
      networking.dhcpcd.allowInterfaces = [
        "enp*"
      ];
    }
    {
      sound.enable = false;
    }
  ];

  krebs.build.host = config.krebs.hosts.test-arch;
}
