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
    # TODO fix krebs.git.rules.[definition 2-entry 2].lass not defined
    #<stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/dcso-vpn.nix>
    <stockholm/lass/2configs/virtualbox.nix>
    <stockholm/lass/2configs/dcso-dev.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/rtl-sdr.nix>
    { # automatic hardware detection
      boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" = {
        device = "/dev/pool/root";
        fsType = "btrfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/1F60-17C6";
        fsType = "vfat";
      };

      fileSystems."/home" = {
        device = "/dev/pool/home";
        fsType = "btrfs";
      };

      fileSystems."/tmp" = {
        device = "tmpfs";
        fsType = "tmpfs";
        options = ["nosuid" "nodev" "noatime"];
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
    {
      #urban terror port
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 27960"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 27960"; target = "ACCEPT"; }
      ];
    }
  ];
  krebs.build.host = config.krebs.hosts.helios;

  krebs.git.rules = [
    {
      user = [ config.krebs.users.lass-helios ];
      repo = [ config.krebs.git.repos.stockholm ];
      perm = with git; push "refs/heads/*" [ fast-forward non-fast-forward create delete merge ];
    }
    {
      lass.umts = {
        enable = true;
        modem = "/dev/serial/by-id/usb-Lenovo_F5521gw_2C7D8D7C35FC7040-if09";
        initstrings = ''
          Init1 = AT+CFUN=1
          Init2 = AT+CGDCONT=1,"IP","pinternet.interkom.de","",0,0
        '';
      };
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

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.xrandrHeads = [
    { output = "DP-2"; primary = true; }
    { output = "DP-4"; monitorConfig = ''Option "Rotate" "left"''; }
    { output = "DP-0"; }
  ];

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output DP-6 --off --output DP-5 --off --output DP-4 --mode 2560x1440 --pos 3840x0 --rotate left --output DP-3 --off --output DP-2 --primary --mode 3840x2160 --pos 0x400 --rotate normal --output DP-1 --off --output DP-0 --mode 2560x1440 --pos 5280x1120 --rotate normal
  '';

  networking.hostName = lib.mkForce "BLN02NB0162";

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

  programs.adb.enable = true;
  users.users.mainUser.extraGroups = [ "adbusers" "docker" ];

  services.printing.drivers = [ pkgs.postscript-lexmark ];

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  virtualisation.docker.enable = true;
}
