{ config, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <stockholm/krebs/2configs/secret-passwords.nix>
    <stockholm/krebs/2configs/hw/x220.nix>

    <stockholm/krebs/2configs/binary-cache/nixos.nix>
    <stockholm/krebs/2configs/binary-cache/prism.nix>
    <stockholm/krebs/2configs/go.nix>
    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/krebs/2configs/news.nix>
    <stockholm/krebs/2configs/news-spam.nix>
    <stockholm/krebs/2configs/shack/ssh-keys.nix>
    <stockholm/krebs/2configs/shack/prometheus/node.nix>
    <stockholm/krebs/2configs/shack/prometheus/server.nix>
    <stockholm/krebs/2configs/shack/prometheus/blackbox.nix>
    <stockholm/krebs/2configs/shack/prometheus/unifi.nix>
    <stockholm/krebs/2configs/shack/prometheus/alertmanager-telegram.nix>
    <stockholm/krebs/2configs/shack/gitlab-runner.nix>

    ## Collect local statistics via collectd and send to collectd
    <stockholm/krebs/2configs/stats/shack-client.nix>
    <stockholm/krebs/2configs/stats/shack-debugging.nix>
  ];

  krebs.build.host = config.krebs.hosts.puyak;
  sound.enable = false;

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    initrd.luks.devices.luksroot.device = "/dev/sda3";
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
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


  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="8c:70:5a:b2:84:58", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="3c:97:0e:07:b9:14", NAME="et0"
  '';

  environment.systemPackages = [ pkgs.zsh ];

  system.activationScripts."disengage fancontrol" = ''
    echo level disengaged > /proc/acpi/ibm/fan
  '';

  users.users.joerg = {
    openssh.authorizedKeys.keys = [ config.krebs.users.Mic92.pubkey ];
    isNormalUser = true;
    shell = "/run/current-system/sw/bin/zsh";
  };
  networking.firewall.allowedTCPPorts = [ 5901 ];
}
