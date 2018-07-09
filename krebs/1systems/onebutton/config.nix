{ config, pkgs, lib, ... }:
{
  # :l <nixpkgs>
  # builtins.readDir (pkgs.fetchFromGitHub { owner = "nixos"; repo = "nixpkgs-channels"; rev = "6c064e6b"; sha256 = "1rqzh475xn43phagrr30lb0fd292c1s8as53irihsnd5wcksnbyd"; })
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    { # flag to rebuild everything yourself:
      # environment.noXlibs = true;

      # minimal disk usage
      nix.gc.automatic = true;
      nix.gc.dates = "03:10";
      documentation.man.enable = false;
      documentation.info.enable = false;
      services.nixosManual.enable = false;
      services.journald.extraConfig = "SystemMaxUse=50M";
    }
    {
      systemd.services.mpc-booter = let
        mpc = "${pkgs.mpc_cli}/bin/mpc -h mpd.shack -p 6600";
        url = "http://lassul.us:8000/radio.ogg";
        say = pkgs.writeDash "say" ''
          tmpfile=$(${pkgs.coreutils}/bin/mktemp)
          echo "$@" > $tmpfile
          ${pkgs.curl}/bin/curl -i -H "content-type: text/plain" -X POST -d "@$tmpfile" gobbelz.shack/say/
          rm "$tmpfile"
        '';
      in {
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        serviceConfig = {
          RemainAfterExit = "yes";
          Type = "oneshot";
          ExecStart = pkgs.writeDash "mpc-boot" ''
            until ${mpc} stats;do
              echo "waiting for mpd.shack to appear"
              sleep 1
            done
            ${say} "Willkommen im Shack wertes Mitglied"

            ${say} "Lassulus Super Radio wurde gestartet"
            ${mpc} add ${url}
            ${mpc} play
          '';
        };
      };
    }
  ];
  krebs.build.host = config.krebs.hosts.onebutton;
  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;

  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_rpi;

  nix.binaryCaches = [ "http://nixos-arm.dezgeg.me/channel" ];
  nix.binaryCachePublicKeys = [ "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%" ];

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  swapDevices = [ { device = "/swapfile"; size = 1024; } ];
  services.openssh.enable = true;

  networking.wireless.enable = true;
  hardware.enableRedistributableFirmware = true;
}
