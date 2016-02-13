{ config, lib, pkgs, ... }:

let
  # XXX cannot use config.build.host.name here because infinite recursion when
  # defining  krebs.hosts.${host-name}.nets.retiolum.aliases  below.
  host-name = "xu";
in

# usage:
#   echo set_password vnc correcthorze | xu-qemu0-monitor
#
#   vncdo -s xu:1 type 'curl init.xu.r' key shift-\\ type sh key return
#
#   http://vnc.xu/vnc_auto.html?port=5701&host=xu&password=correcthorze
#
#   make [install] system=xu-qemu0 target_host=10.56.0.101

# TODO iptables -A INPUT -p udp -m udp --dport bootps -j ACCEPT
# TODO iptables -A FORWARD -i qemubr0 -s 10.56.0.1/24 -m conntrack --ctstate NEW -j ACCEPT
# TODO iptables -A POSTROUTING -t nat -j MASQUERADE
# TODO iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
# TODO iptables -A INPUT -i qemubr0 -p udp -m udp --dport domain -j ACCEPT
# TODO echo 1 > /proc/sys/net/ipv4/ip_forward
# TODO ifconfig qemubr0 10.56.0.1/24 up

with lib;

{
  #networking.wireless.interfaces = [ "wlp3s0" ];

  #networking.useNetworkd = true;

  #networking.dhcpcd.allowInterfaces = [
  #  "qemubr0"
  #];

  #systemd.network.networks.wlp3s0 = {
  #  matchConfig.name = "wlp3s0";
  #  networkConfig.Bridge = "qemubr0";
  #};

  systemd.network.enable = true;

  systemd.network.netdevs.qemubr0 = {
    netdevConfig = {
      Name = "qemubr0";
      Kind = "bridge";
    };
  };

  users.groups.qemu-users.gid = genid "qemu-users";

  environment.etc."qemu/bridge.conf".text = ''
    allow qemubr0
  '';

  krebs.per-user.tv.packages = [
    pkgs.vncdotool
  ];

  users.users.xu-qemu0 = {
    createHome = true;
    group = "qemu-users";
    home = "/home/xu-qemu0";
    uid = genid "xu-qemu0";
  };

  systemd.services.xu-qemu0 = let
  in {
    after = [ "network.target" "systemd-resolved.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "xu-qemu0";
      SyslogIdentifier = "xu-qemu0";
      ExecStart = pkgs.writeDash "xu-qemu0" ''
        set -efu
        img=$HOME/tmp/xu-qemu0.raw
        if ! test -e "$img"; then
          ${pkgs.coreutils}/bin/mkdir -p "$(${pkgs.coreutils}/bin/dirname "$img")"
          ${pkgs.kvm}/bin/qemu-img create "$img" 10G
        fi
        exec ${pkgs.kvm}/bin/qemu-kvm \
            -monitor unix:$HOME/xu-qemu0.sock,server,nowait \
            -boot order=cd \
            -cdrom ${pkgs.fetchurl {
              url = https://nixos.org/releases/nixos/15.09/nixos-15.09.1012.9fe0c23/nixos-minimal-15.09.1012.9fe0c23-x86_64-linux.iso;
              sha256 = "18bc9wrsrjnhj9rya75xliqkl99gxbsk4dmwqivhvwfzb5qb5yp9";
            }} \
            -m 1024 \
            -netdev bridge,br=qemubr0,id=hn0,helper=/var/setuid-wrappers/qemu-bridge-helper \
            -net nic,netdev=hn0,id=nic1,macaddr=52:54:00:12:34:56 \
            -drive file="$img",format=raw \
            -display vnc=:1,websocket=5701,password,lossy \
            -name xu-qemu0 \
      '';
    };
  };

  system.activationScripts."krebs.setuid.xu-qemu0-monitor" = stringAfter [ "setuid" ] ''
    src=${pkgs.execve "xu-qemu0-monitor" {
      # TODO toC should handle derivation, then we don't have to "${...}" here
      filename = "${pkgs.writeDash "xu-qemu0-monitor" ''
        exec ${pkgs.socat}/bin/socat \
            stdio \
            UNIX-CONNECT:${config.users.users.xu-qemu0.home}/xu-qemu0.sock \
      ''}";
    }}
    dst=${config.security.wrapperDir}/xu-qemu0-monitor
    cp "$src" "$dst"
    chown xu-qemu0.tv "$dst"
    chmod 4710 "$dst"
  '';

  #TODO krebs.setuid.qemu-bridge-helper = {
  #  filename = "${pkgs.qemu}/libexec/qemu-bridge-helper";
  #  owner = "root";
  #  group = "qemu-users";
  #  mode = "4710";
  #};
  system.activationScripts."krebs.setuid" = stringAfter [ "setuid" ] ''
    src=${pkgs.execve "qemu-bridge-helper" {
      filename = "${pkgs.qemu}/libexec/qemu-bridge-helper";
    }}
    dst=${config.security.wrapperDir}/qemu-bridge-helper
    cp "$src" "$dst"
    chown root.qemu-users "$dst"
    chmod 4710 "$dst"
  '';

  users.users.qemu-dnsmasq.uid = genid "qemu-dnsmasq";

  # TODO need custom etc/dbus-1/system.d/dnsmasq.conf for different BusName
  services.dbus.packages = [ pkgs.dnsmasq ];

  systemd.services.qemu-dnsmasq = let
    # bind-interfaces
    conf = pkgs.writeText "qemu-dnsmasq.conf" ''
      listen-address=10.56.0.1
      interface=qemubr0
      dhcp-range=10.56.0.200,10.56.0.250
      dhcp-no-override
      dhcp-leasefile=/tmp/qemu-dnsmasq.leases
      domain=${host-name}.local
      dhcp-host=52:54:00:12:34:56,xu-qemu0,10.56.0.101,1440m
    '';
  in {
    after = [ "network.target" "systemd-resolved.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "dbus";
      BusName = "uk.org.thekelleys.dnsmasq";
      # -1 --enable-dbus[=uk.org.thekelleys.dnsmasq]
      SyslogIdentifier = "qemu-dnsmasq";
      ExecStart = "${pkgs.dnsmasq}/bin/dnsmasq -1k -u qemu-dnsmasq -C ${conf}";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      PrivateTmp = "true";
    };
    restartTriggers = [ config.environment.etc.hosts.source ];
  };


  krebs.nginx.servers.init = {
    server-names = [
      "init.${host-name}"
      "init.${host-name}.r"
      "init.${host-name}.retiolum"
    ];
    extraConfig = ''
      index init.txt;
      root ${pkgs.writeTextFile {
        name = "init-pages";
        text = ''
          #! /bin/sh
          set -efu

          dev=/dev/sda
          pttype=dos # gpt

          case $pttype in
            dos)
              if ! test "$(blkid -o value -s PTTYPE "$dev")" = dos; then
                parted -s "$dev" mklabel msdos
              fi
              if ! test "$(blkid -o value -s PARTLABEL "$dev"1)" = primary; then
                parted -s "$dev" mkpart primary ext4 1MiB 513MiB
                parted -s "$dev" set 1 boot on
              fi
              ;;
            gpt)
              if ! test "$(blkid -o value -s PTTYPE "$dev")" = gpt; then
                parted -s "$dev" mklabel gpt
              fi
              if ! test "$(blkid -o value -s PARTLABEL "$dev"1)" = ESP; then
                parted -s "$dev" mkpart ESP fat32 1MiB 513MiB
                parted -s "$dev" set 1 boot on
              fi
              ;;
            *)
              echo "Error: bad pttype: $pttype" >&2
              exit -1
          esac

          if ! test "$(blkid -o value -s PARTLABEL "$dev"2)" = primary; then
            parted -s "$dev" mkpart primary btrfs 513MiB 100%
          fi
          if ! test "$(blkid -o value -s TYPE "$dev"1)" = vfat; then
            mkfs.vfat "$dev"1
          fi
          if ! test "$(blkid -o value -s TYPE "$dev"2)" = btrfs; then
            mkfs.btrfs "$dev"2
          fi

          parted "$dev" print

          if ! test "$(lsblk -n -o MOUNTPOINT "$dev"2)" = /mnt; then
            mount "$dev"2 /mnt
          fi
          if ! test "$(lsblk -n -o MOUNTPOINT "$dev"1)" = /mnt/boot; then
            mkdir -m 0000 -p /mnt/boot
            mount "$dev"1 /mnt/boot
          fi

          lsblk "$dev"

          key=${shell.escape config.krebs.users.tv-xu.pubkey}

          if [ "$(cat /root/.ssh/authorized_keys 2>/dev/null)" != "$key" ]; then
            mkdir -p /root/.ssh
            echo "$key" > /root/.ssh/authorized_keys
          fi
          systemctl start sshd
          ip route
          echo READY.
        '';
        destination = "/init.txt";
      }};
    '';
  };


  krebs.hosts.${host-name}.nets.retiolum.aliases = [
    "init.${host-name}.r"
    "init.${host-name}.retiolum"
    "vnc.${host-name}.r"
    "vnc.${host-name}.retiolum"
  ];

  krebs.nginx.servers.noVNC = {
    server-names = [
      "vnc.${host-name}"
      "vnc.${host-name}.r"
      "vnc.${host-name}.retiolum"
    ];
    #rewrite ^([^.]*)$ /vnc_auto.html?host=localhost&port=5701;
    locations = singleton (nameValuePair "/" ''
      index vnc.html;
      root ${pkgs.noVNC};
    '');
  };
}
