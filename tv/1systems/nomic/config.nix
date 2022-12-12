with import ./lib;
{ config, pkgs, ... }: {
  krebs.build.host = config.krebs.hosts.nomic;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/gitrepos.nix>
    <stockholm/tv/2configs/mail-client.nix>
    <stockholm/tv/2configs/nginx/public_html.nix>
    <stockholm/tv/2configs/pulse.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xserver>
  ];

  boot.initrd.luks.devices.luks1.device = "/dev/sda2";

  # Don't use UEFI because current disk was partitioned/formatted for AO753.
  # TODO remove following bool.loader section after repartitioning/reformatting
  boot.loader = {
    grub = {
      device = "/dev/sda";
      splashImage = null;
    };
    systemd-boot.enable = mkForce false;
  };

  fileSystems."/" =
    { device = "/dev/mapper/nomic1-root";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/sda1";
      fsType = "ext4";
    };

  fileSystems."/home" =
    { device = "/dev/mapper/nomic1-home";
      fsType = "btrfs";
    };

  environment.homeBinInPath = true;

  environment.systemPackages = with pkgs; [
    (writeDashBin "play" ''
      set -euf
      mpv() { exec ${mpv}/bin/mpv "$@"; }
      case $1 in
        deepmix)      mpv http://deepmix.ru/deepmix128.pls;;
        groovesalad)  mpv http://somafm.com/play/groovesalad;;
        ntslive)      mpv http://listen2.ntslive.co.uk/listen.pls;;
        *)
          echo "$0: bad argument: $*" >&2
          exit 23
      esac
    '')
    gnupg
    tmux
  ];

  networking.wireless.enable = true;
}
