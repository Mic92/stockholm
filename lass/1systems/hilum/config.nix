{ config, ... }:
{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/network-manager.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];

  krebs.build.host = config.krebs.hosts.hilum;

  boot.loader.grub.extraEntries = ''
    menuentry "grml" {
      iso_path=/isos/grml.iso
      export iso_path
      search --set=root --file $iso_path
      loopback loop $iso_path
      root=(loop)
      configfile /boot/grub/loopback.cfg
      loopback --delete loop
    }
  '';
}
