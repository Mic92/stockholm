{ config, pkgs, lib, ... }:
{
  imports = [
    ./net.nix
    ../../../krebs
    ../../../krebs/2configs
    ../../2configs/secret-passwords.nix
    ../../2configs/hw/x220.nix

    # see documentation in included getty-for-esp.nix:
    # brain hosts/puyak/root
    ../../2configs/hw/getty-for-esp.nix

    ../../2configs/buildbot/worker.nix

    ## initrd unlocking
    # (brain hosts/puyak/luks-ssd;echo)  | ssh root@$(brain krebs-secrets/puyak/initrd/hostname) 'cat  /crypt-ramfs/passphrase'
    ../../2configs/tor/initrd.nix

    ../../2configs/binary-cache/nixos.nix
    ../../2configs/binary-cache/prism.nix

    ## news host

    ../../2configs/container-networking.nix
    ../../2configs/syncthing.nix

    ### shackspace ###
    # handle the worlddomination map via coap
    ../../2configs/shack/worlddomination.nix
    ../../2configs/shack/ssh-keys.nix

    # drivedroid.shack for shackphone
    ../../2configs/shack/drivedroid.nix
    # ../../2configs/shack/nix-cacher.nix

    # Say if muell will be collected
    ../../2configs/shack/muell_caller.nix
    # provide muellshack api: muell.shack
    ../../2configs/shack/muellshack.nix
    # send mail if muell was not handled
    ../../2configs/shack/muell_mail.nix

    # provide light control api
    ../../2configs/shack/node-light.nix # light.shack lounge.light.shack power.light.shack openhab.shack lightapi.shack
    # light.shack web-ui
    ../../2configs/shack/light.shack.nix #light.shack

    # fetch the u300 power stats
    ../../2configs/shack/power/u300-power.nix


    { # do not log to /var/spool/log
      services.nginx.appendHttpConfig = ''
          map $request_method $loggable {
            default 1;
            GET 0;
          }
          log_format vhost '$host $remote_addr - $remote_user '
                     '[$time_local] "$request" $status '
                     '$body_bytes_sent "$http_referer" '
                     '"$http_user_agent"';
          error_log stderr;
          access_log syslog:server=unix:/dev/log vhost;
      '';
      services.journald.rateLimitBurst = 10000;
    }

    # create samba share for anonymous usage with the laser and 3d printer pc
    ../../2configs/shack/share.nix

    # mobile.lounge.mpd.shack
    ../../2configs/shack/mobile.mpd.nix

    # hass.shack
    ../../2configs/shack/glados
    ../../2configs/shack/esphome.nix

    # connect to git.shackspace.de as group runner for rz
    ../../2configs/shack/gitlab-runner.nix

    # Statistics collection and visualization
    # ../../2configs/shack/graphite.nix # graphiteApi is broken and unused(hopefully)
    ## Collect data from mqtt.shack and store in graphite database
    ../../2configs/shack/mqtt_sub.nix
    ## Collect radioactive data and put into graphite
    ../../2configs/shack/radioactive.nix
    ## mqtt.shack
    ../../2configs/shack/mqtt.nix
    ## influx.shack
    ../../2configs/shack/influx.nix

    ## Collect local statistics via collectd and send to collectd
    # ../../2configs/stats/shack-client.nix
    # ../../2configs/stats/shack-debugging.nix

    ## netbox.shack: Netbox is disabled as nobody seems to be using it anyway
    # ../../2configs/shack/netbox.nix

    # grafana.shack
    ../../2configs/shack/grafana.nix

    # shackdns.shack
    # replacement for leases.shack and shackles.shack
    ../../2configs/shack/shackDNS.nix

    # monitoring: prometheus.shack
    ../../2configs/shack/prometheus/node.nix
    ../../2configs/shack/prometheus/server.nix
    ../../2configs/shack/prometheus/blackbox.nix
    #../../2configs/shack/prometheus/unifi.nix
    # TODO: alertmanager 0.24+ supports telegram
    # ../../2configs/shack/prometheus/alertmanager-telegram.nix
  ];

  krebs.build.host = config.krebs.hosts.puyak;
  krebs.hosts.puyak.ssh.privkey.path = "${config.krebs.secret.directory}/ssh.id_ed25519";

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    initrd.luks.devices.luksroot.device = "/dev/sda3";
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];

    kernelModules = [ "kvm-intel" ];
    extraModprobeConfig = ''
      options thinkpad_acpi fan_control=1
    '';
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/pool-root";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/boot" = {
      device = "/dev/sda2";
    };
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/home" = {
      device = "/dev/mapper/pool-home";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };

  services.logind.lidSwitch = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";



  environment.systemPackages = [ pkgs.zsh ];

  system.activationScripts."disengage fancontrol" = ''
    echo level disengaged > /proc/acpi/ibm/fan
  '';

  users.users.joerg = {
    openssh.authorizedKeys.keys = [ config.krebs.users.mic92.pubkey ];
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";
  };
  system.stateVersion = lib.mkForce "24.05";
}
