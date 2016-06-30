{ config, lib, pkgs, ... }:

{
  imports = [
    ../.
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/default.nix
    #../2configs/exim-retiolum.nix
    ../2configs/git.nix
    {
      boot.loader.grub = {
        device = "/dev/vda";
        splashImage = null;
      };

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "ehci_pci"
        "uhci_hcd"
        "virtio_pci"
        "virtio_blk"
      ];

      fileSystems."/" = {
        device = "/dev/mapper/pool-nix";
        fsType = "ext4";
      };

      fileSystems."/srv/http" = {
        device = "/dev/pool/srv_http";
        fsType = "ext4";
      };

      fileSystems."/boot" = {
        device = "/dev/vda1";
        fsType = "ext4";
      };
      fileSystems."/bku" = {
        device = "/dev/pool/bku";
        fsType = "ext4";
      };
    }
    {
      networking.dhcpcd.allowInterfaces = [
        "enp*"
        "eth*"
      ];
    }
    {
      sound.enable = false;
    }
    {
      environment.systemPackages = with pkgs; [
        mk_sql_pair
      ];
    }
    {
      imports = [
        ../2configs/websites/fritz.nix
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
         { predicate = "-p tcp --dport https"; target = "ACCEPT"; }
      ];
    }
    {
      #TODO: abstract & move to own file
      krebs.exim-smarthost = {
        enable = true;
        relay_from_hosts = map (host: host.nets.retiolum.ip4.addr) [
          config.krebs.hosts.mors
          config.krebs.hosts.uriel
          config.krebs.hosts.helios
        ];
        system-aliases = [
          { from = "mailer-daemon"; to = "postmaster"; }
          { from = "postmaster"; to = "root"; }
          { from = "nobody"; to = "root"; }
          { from = "hostmaster"; to = "root"; }
          { from = "usenet"; to = "root"; }
          { from = "news"; to = "root"; }
          { from = "webmaster"; to = "root"; }
          { from = "www"; to = "root"; }
          { from = "ftp"; to = "root"; }
          { from = "abuse"; to = "root"; }
          { from = "noc"; to = "root"; }
          { from = "security"; to = "root"; }
          { from = "root"; to = "lass"; }
        ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport smtp"; target = "ACCEPT"; }
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.dishfire;
}
