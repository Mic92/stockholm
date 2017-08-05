{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs = {
    enable = true;
    build = {
      user = config.krebs.users.mv;
      host = config.krebs.hosts.stro;
      source = let
        HOME = getEnv "HOME";
        host = config.krebs.build.host;
      in {
        nixos-config.symlink = "stockholm/mv/1systems/${host.name}.nix";
        secrets.file = "${HOME}/secrets/${host.name}";
        stockholm.file = "${HOME}/stockholm";
        nixpkgs.git = {
          url = https://github.com/NixOS/nixpkgs;
          ref = "8bf31d7d27cae435d7c1e9e0ccb0a320b424066f";
        };
      };
    };
  };

  imports = [
    <secrets>
    <stockholm/krebs>
    <stockholm/tv/2configs/audit.nix>
    <stockholm/tv/2configs/bash.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/im.nix>
    <stockholm/tv/2configs/mail-client.nix>
    <stockholm/tv/2configs/nginx/public_html.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/ssh.nix>
    <stockholm/tv/2configs/sshd.nix>
    <stockholm/tv/2configs/vim.nix>
    <stockholm/tv/2configs/xdg.nix>
    <stockholm/tv/2configs/xserver>
    <stockholm/tv/3modules>
    <stockholm/tv/5pkgs>
  ];

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  boot.initrd.luks = {
    cryptoModules = [ "aes" "sha512" "xts" ];
    devices = [
      {
        name = "luks1";
        device = "/dev/disk/by-id/ata-TOSHIBA-TR150_467B50JXK8WU-part2";
      }
    ];
  };

  environment = {
    profileRelativeEnvVars.PATH = mkForce [ "/bin" ];
    shellAliases = mkForce {
      gp = "${pkgs.pari}/bin/gp -q";
      df = "df -h";
      du = "du -h";
      ls = "ls -h --color=auto --group-directories-first";
      dmesg = "dmesg -L --reltime";
      view = "vim -R";

      reload = "systemctl reload";
      restart = "systemctl restart";
      start = "systemctl start";
      status = "systemctl status";
      stop = "systemctl stop";
    };
    systemPackages = with pkgs; [
      dic
      htop
      p7zip
      q

      pavucontrol
      rxvt_unicode.terminfo

      # stockholm
      git
      gnumake
      populate
    ];
    variables = {
      NIX_PATH = mkForce "secrets=/var/src/stockholm/null:/var/src";
    };
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-id/ata-TOSHIBA-TR150_467B50JXK8WU-part1";
    };
    "/" = {
      device = "/dev/mapper/vg1-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/vg1-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };

  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
  };

  networking.hostName = config.krebs.build.host.name;

  nix = {
    binaryCaches = ["https://cache.nixos.org"];
    # TODO check if both are required:
    chrootDirs = [ "/etc/protocols" pkgs.iana_etc.outPath ];
    requireSignedBinaryCaches = true;
    useChroot = true;
  };

  nixpkgs.config.allowUnfree = false;

  users = {
    defaultUserShell = "/run/current-system/sw/bin/bash";
    mutableUsers = false;
    users = {
      mv = {
        inherit (config.krebs.users.mv) home uid;
        isNormalUser = true;
      };
    };
  };

  security.sudo.extraConfig = ''
    Defaults env_keep+="SSH_CLIENT"
    Defaults mailto="${config.krebs.users.mv.mail}"
    Defaults !lecture
  '';

  services.cron.enable = false;
  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';
  services.nscd.enable = false;
  services.ntp.enable = false;
  services.timesyncd.enable = true;

  time.timeZone = "Europe/Berlin";

  tv.iptables = {
    enable = true;
    accept-echo-request = "internet";
  };

  system.stateVersion = "16.03";
}
