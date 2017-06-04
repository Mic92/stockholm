{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.build.host = config.krebs.hosts.nomic;

  imports = [
    ../.
    ../2configs/hw/x220.nix
    ../2configs/exim-retiolum.nix
    ../2configs/gitrepos.nix
    ../2configs/im.nix
    ../2configs/mail-client.nix
    ../2configs/nginx/public_html.nix
    ../2configs/pulse.nix
    ../2configs/retiolum.nix
    ../2configs/xserver
  ];

  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "xts" ];
    devices = [
      { name = "luks1"; device = "/dev/sda2"; }
    ];
  };

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

  swapDevices = [ ];

  # TODO base
  boot.tmpOnTmpfs = true;

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
}
