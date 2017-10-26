with import <stockholm/lib>;
{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/otp-ssh.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/dcso-vpn.nix>
    { # automatic hardware detection
      boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" =
        { device = "/dev/pool/root";
          fsType = "btrfs";
        };

      fileSystems."/boot" =
        { device = "/dev/disk/by-uuid/1F60-17C6";
          fsType = "vfat";
        };

      fileSystems."/home" =
        { device = "/dev/pool/home";
          fsType = "btrfs";
        };

      nix.maxJobs = lib.mkDefault 8;
    }
    { # crypto stuff
      boot.initrd.luks = {
        cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
        devices =  [{
           name = "luksroot";
           device = "/dev/nvme0n1p3";
        }];
      };
    }
    {
      services.xserver.dpi = 200;
      fonts.fontconfig.dpi = 200;
      lass.fonts.regular = "xft:Hack-Regular:pixelsize=22,xft:Symbola";
      lass.fonts.bold =    "xft:Hack-Bold:pixelsize=22,xft:Symbola";
      lass.fonts.italic =  "xft:Hack-RegularOblique:pixelsize=22,xft:Symbol";
    }
    { #TAPIR, AGATIS, sentral, a3 - foo
      services.redis.enable = true;
    }
    {
      krebs.fetchWallpaper = {
        enable = true;
        url = "http://i.imgur.com/0ktqxSg.png";
        maxTime = 9001;
      };
    }
  ];
  krebs.build.host = config.krebs.hosts.helios;

  krebs.git.rules = [
    {
      user = [ config.krebs.users.lass-helios ];
      repo = [ config.krebs.git.repos.stockholm ];
      perm = with git; push "refs/heads/*" [ fast-forward non-fast-forward create delete merge ];
    }
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.wireless.enable = true;
  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    ag
    vim
    rxvt_unicode
    git
    rsync
    hashPassword
    thunderbird
    dpass
  ];

  users.users = {
    root.openssh.authorizedKeys.keys = [
      config.krebs.users.lass-helios.pubkey
    ];
  };

  programs.ssh.startAgent = lib.mkForce true;

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.xrandrHeads = [
    { output = "DP-0.8"; }
    { output = "DP-4"; monitorConfig = ''Option "Rotate" "right"''; }
    { output = "DP-2"; primary = true; }
  ];

  security.pki.certificateFiles = [
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC1G1.pem"; sha256 = "14vz9c0fk6li0a26vx0s5ha6y3yivnshx9pjlh9vmnpkbph5a7rh"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC2G1.pem"; sha256 = "0r1dd48a850cv7whk4g2maik550rd0vsrsl73r6x0ivzz7ap1xz5"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAROOTC3G1.pem"; sha256 = "0b5cdchdkvllnr0kz35d8jrmrf9cjw0kd98mmvzr0x6nkc8hwpdy"; })

   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC2G1.pem"; sha256 = "0rn57zv1ry9vj4p2248mxmafmqqmdhbrfx1plszrxsphshbk2hfz"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCACOMPC3G1.pem"; sha256 = "0w88qaqhwxzvdkx40kzj2gka1yi85ipppjdkxah4mscwfhlryrnk"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC2G1.pem"; sha256 = "1z2qkyhgjvri13bvi06ynkb7mjmpcznmc9yw8chx1lnwc3cxa7kf"; })
   (pkgs.fetchurl { url = "http://pki.dcso.de/ca/PEM/DCSOCAIDENC3G1.pem"; sha256 = "0smdjjvz95n652cb45yhzdb2lr83zg52najgbzf6lm3w71f8mv7f"; })
  ];

  lass.screenlock.command = "${pkgs.i3lock}/bin/i3lock -i /home/lass/lock.png -t -f";
}
