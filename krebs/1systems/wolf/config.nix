{ config, pkgs, ... }:
let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
  ext-if = "et0";
  external-mac = "52:54:b0:0b:af:fe";

in
{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>

    <stockholm/krebs/2configs/binary-cache/nixos.nix>
    <stockholm/krebs/2configs/binary-cache/prism.nix>

    # handle the worlddomination map via coap
    <stockholm/krebs/2configs/shack/worlddomination.nix>
    <stockholm/krebs/2configs/shack/ssh-keys.nix>

    # drivedroid.shack for shackphone
    <stockholm/krebs/2configs/shack/drivedroid.nix>
    # <stockholm/krebs/2configs/shack/nix-cacher.nix>
    # Say if muell will be collected
    <stockholm/krebs/2configs/shack/muell_caller.nix>
    # provide muellshack api
    <stockholm/krebs/2configs/shack/muellshack.nix>
    # provide light control api
    <stockholm/krebs/2configs/shack/node-light.nix>
    # send mail if muell was not handled
    <stockholm/krebs/2configs/shack/muell_mail.nix>
    # send mail if muell was not handled
    <stockholm/krebs/2configs/shack/s3-power.nix>
    # powerraw usb serial to mqtt and raw socket
    <stockholm/krebs/2configs/shack/powerraw.nix>

    # create samba share for anonymous usage with the laser and 3d printer pc
    <stockholm/krebs/2configs/shack/share.nix>

    # mobile.lounge.mpd.shack
    <stockholm/krebs/2configs/shack/mobile.mpd.nix>

    # hass.shack
    <stockholm/krebs/2configs/shack/glados>

    # connect to git.shackspace.de as group runner for rz
    <stockholm/krebs/2configs/shack/gitlab-runner.nix>

    # Statistics collection and visualization
    # <stockholm/krebs/2configs/shack/graphite.nix> # graphiteApi is broken and unused(hopefully)
    ## Collect data from mqtt.shack and store in graphite database
    <stockholm/krebs/2configs/shack/mqtt_sub.nix>
    ## Collect radioactive data and put into graphite
    <stockholm/krebs/2configs/shack/radioactive.nix>
    ## mqtt.shack
    <stockholm/krebs/2configs/shack/mqtt.nix>
    ## influx.shack
    <stockholm/krebs/2configs/shack/influx.nix>

    ## Collect local statistics via collectd and send to collectd
    <stockholm/krebs/2configs/stats/shack-client.nix>
    <stockholm/krebs/2configs/stats/shack-debugging.nix>

    <stockholm/krebs/2configs/shack/netbox.nix>
    # prometheus.shack
    #<stockholm/krebs/2configs/shack/prometheus/server.nix>
    <stockholm/krebs/2configs/shack/prometheus/node.nix>
    #<stockholm/krebs/2configs/shack/prometheus/unifi.nix>
    # grafana.shack
    <stockholm/krebs/2configs/shack/grafana.nix>

    # shackdns.shack
    # replacement for leases.shack and shackles.shack
    <stockholm/krebs/2configs/shack/shackDNS.nix>

  ];
  # use your own binary cache, fallback use cache.nixos.org (which is used by
  # apt-cacher-ng in first place)

  # local discovery in shackspace
  nixpkgs.config.packageOverrides = pkgs: { tinc = pkgs.tinc_pre; };
  krebs.tinc.retiolum.extraConfig = "TCPOnly = yes";


  networking = {
    firewall.enable = false;
    firewall.allowedTCPPorts = [ 8088 8086 8083 ];
    interfaces."${ext-if}".ipv4.addresses = [
      {
        address = shack-ip;
        prefixLength = 20;
      }
    ];

    defaultGateway = "10.42.0.1";
    nameservers = [ "10.42.0.100" "10.42.0.200" ];
  };

  #####################
  # uninteresting stuff
  #####################
  krebs.build.host = config.krebs.hosts.wolf;

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  boot.initrd.availableKernelModules = [
    "ata_piix" "uhci_hcd" "ehci_pci" "virtio_pci" "virtio_blk"
  ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  # without it `/nix/store` is not added grub paths
  boot.loader.grub.copyKernels = true;

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="${external-mac}", NAME="${ext-if}"
  '';

  time.timeZone = "Europe/Berlin";
  sound.enable = false;

  # avahi
  services.avahi = {
    enable = true;
    wideArea = false;
  };

  environment.systemPackages = [ pkgs.avahi ];

}
