with import <stockholm/lib>;
{ config, pkgs, ... }: let
  cfg = config.krebs.sync-containers;
  paths = cname: {
    plain = "/var/lib/containers/${cname}/var/state";
    ecryptfs = "${cfg.dataLocation}/${cname}/ecryptfs";
    securefs = "${cfg.dataLocation}/${cname}/securefs";
    luksfile = "${cfg.dataLocation}/${cname}/luksfile";
  };
  init = cname: {
    plain = ''
      echo 'no need for init'
    '';
    ecryptfs = ''
      ${pkgs.ecrypt}/bin/ecrypt init ${cfg.dataLocation}/${cname}/ecryptfs /var/lib/containers/${cname}/var/state
    '';
    securefs = ''
      ${pkgs.securefs}/bin/securefs create --format 3 ${cfg.dataLocation}/${cname}/securefs
    '';
    luksfile = ''
      ${pkgs.coreutils}/bin/truncate -s 10G '${(paths cname).luksfile}/fs.luks'
      ${pkgs.cryptsetup}/bin/cryptsetup luksFormat '${(paths cname).luksfile}/fs.luks'
      ${pkgs.cryptsetup}/bin/cryptsetup luksOpen '${(paths cname).luksfile}/fs.luks' 'luksfile-${cname}'
      ${pkgs.xfsprogs}/bin/mkfs.xfs '/dev/mapper/luksfile-${cname}'
    '';
  };
  start = cname: {
    plain = ''
      :
    '';
    ecryptfs = ''

      if [ -e ${cfg.dataLocation}/${cname}/ecryptfs/.cfg.json ]; then
        if ! mount | grep -q '${cfg.dataLocation}/${cname}/ecryptfs on /var/lib/containers/${cname}/var/state type ecryptfs'; then
          ${pkgs.ecrypt}/bin/ecrypt mount ${cfg.dataLocation}/${cname}/ecryptfs /var/lib/containers/${cname}/var/state
        fi
      else
        echo 'please run init-${cname} first'
        exit 1
      fi
    '';
    securefs = ''
      ## check if FS was initialized first
      if ! ${pkgs.mount}/bin/mount | grep -q '^securefs on /var/lib/containers/${cname}/var/state type fuse.securefs'; then
        ${pkgs.securefs}/bin/securefs mount ${cfg.dataLocation}/${cname}/securefs /var/lib/containers/${cname}/var/state -b -o allow_other -o default_permissions
      fi
    '';
    luksfile = ''
      mkdir -p /var/lib/containers/${cname}/var/state
      if ! test -e /dev/mapper/luksfile-${cname}; then
        ${pkgs.cryptsetup}/bin/cryptsetup luksOpen '${(paths cname).luksfile}/fs.luks' 'luksfile-${cname}'
      fi
      if ! ${pkgs.mount}/bin/mount | grep -q '^/dev/mapper/luksfile-${cname} on /var/lib/containers/${cname}/var/state'; then
        mount '/dev/mapper/luksfile-${cname}' '/var/lib/containers/${cname}/var/state'
      fi
    '';
  };
  stop = cname: {
    plain = ''
      :
    '';
    ecryptfs = ''
      ${pkgs.ecrypt}/bin/ecrypt unmount ${cfg.dataLocation}/${cname}/ecryptfs /var/lib/containers/${cname}/var/state
    '';
    securefs = ''
      umount /var/lib/containers/${cname}/var/state
    '';
    luksfile = ''
      umount /var/lib/containers/${cname}/var/state
      ${pkgs.cryptsetup}/bin/cryptsetup luksClose luksfile-${cname}
    '';
  };
in {
  options.krebs.sync-containers = {
    dataLocation = mkOption {
      description = ''
        location where the encrypted sync-containers lie around
      '';
      default = "/var/lib/sync-containers";
      type = types.absolute-pathname;
    };
    containers = mkOption {
      type = types.attrsOf (types.submodule ({ config, ... }: {
        options = {
          name = mkOption {
            description = ''
              name of the container
            '';
            default = config._module.args.name;
            type = types.str;
          };
          peers = mkOption {
            description = ''
              syncthing peers to share this container with
            '';
            default = [];
            type = types.listOf types.str;
          };
          format = mkOption {
            description = ''
              file system encrption format of the container
            '';
            type = types.enum [ "plain" "ecryptfs" "securefs" "luksfile" ];
          };
        };
      }));
      default = {};
    };
  };

  config = mkIf (cfg.containers != {}) {
    programs.fuse.userAllowOther = true;
    # allow syncthing to enter /var/lib/containers
    system.activationScripts.containers-enter = mkDefault ''
      ${pkgs.coreutils}/bin/chmod a+x /var/lib/containers || :
    '';

    services.syncthing.folders = (mapAttrs' (_: ctr: nameValuePair "${(paths ctr.name).${ctr.format}}" ({
      devices = ctr.peers;
      ignorePerms = false;
    })) cfg.containers);

    krebs.acl = mapAttrs' (_: ctr: nameValuePair "${(paths ctr.name).${ctr.format}}" {
      "u:syncthing:rX".parents = true;
      "u:syncthing:rwX" = {};
    }) cfg.containers;


    systemd.services = mapAttrs' (n: ctr: nameValuePair "containers@${ctr.name}" ({
      reloadIfChanged = mkForce false;
    })) cfg.containers;

    containers = mapAttrs' (n: ctr: nameValuePair ctr.name ({
      config = { ... }: {
        environment.systemPackages = [
          pkgs.dhcpcd
          pkgs.git
          pkgs.jq
        ];
        networking.useDHCP = mkForce true;
        system.activationScripts.fuse = {
          text = ''
            ${pkgs.coreutils}/bin/mknod /dev/fuse c 10 229
          '';
          deps = [];
        };
      };
      allowedDevices = [
        { modifier = "rwm"; node = "/dev/fuse"; }
      ];
      autoStart = false;
      enableTun = true;
      privateNetwork = true;
      hostBridge = "ctr0";
    })) cfg.containers;

    networking.networkmanager.unmanaged = [ "ctr0" ];
    networking.bridges.ctr0.interfaces = [];
    networking.interfaces.ctr0.ipv4.addresses = [{
      address = "10.233.0.1";
      prefixLength = 24;
    }];
    # networking.nat = {
    #   enable = true;
    #   externalInterface = lib.mkDefault "et0";
    #   internalInterfaces = [ "ctr0" ];
    # };
    services.dhcpd4 = {
      enable = true;
      interfaces = [ "ctr0" ];
      extraConfig = ''
        option subnet-mask 255.255.255.0;
        option routers 10.233.0.1;
        # option domain-name-servers 8.8.8.8; # TODO configure dns server
        subnet 10.233.0.0 netmask 255.255.255.0 {
          range 10.233.0.10 10.233.0.250;
        }
      '';
    };

    users.users.root.packages = flatten (mapAttrsToList (n: ctr: [
      (pkgs.writeDashBin "init-${ctr.name}" ''
        set -euf
        set -x

        mkdir -p /var/lib/containers/${ctr.name}/var/state
        STATE=$(/run/current-system/sw/bin/nixos-container status ${ctr.name})
        if [ "$STATE" = 'up' ]; then
          /run/current-system/sw/bin/nixos-container stop ${ctr.name}
        fi
        ${(init ctr.name).${ctr.format}}
        ${(start ctr.name).${ctr.format}}
        /run/current-system/sw/bin/nixos-container start ${ctr.name}
        /run/current-system/sw/bin/nixos-container run ${ctr.name} -- ${pkgs.writeDash "deploy-${ctr.name}" ''
          set -x

          mkdir -p /var/state/var_src
          ln -sfTr /var/state/var_src /var/src
          touch /etc/NIXOS
        ''}
        target_ip=$(/run/current-system/sw/bin/nixos-container run ${ctr.name} -- ip -j a s eth0 | jq -r '.[].addr_info[] | select(.family=="inet") | .local')

        echo "deploy to $target_ip"
      '')
      (pkgs.writeDashBin "start-${ctr.name}" ''
        set -euf
        set -x

        mkdir -p /var/lib/containers/${ctr.name}/var/state

        ${(start ctr.name).${ctr.format}}

        STATE=$(/run/current-system/sw/bin/nixos-container status ${ctr.name})
        if [ "$STATE" = 'down' ]; then
          /run/current-system/sw/bin/nixos-container start ${ctr.name}
        fi

        /run/current-system/sw/bin/nixos-container run ${ctr.name} -- ${pkgs.writeDash "deploy-${ctr.name}" ''
          set -x

          mkdir -p /var/state/var_src
          ln -sfTr /var/state/var_src /var/src
          touch /etc/NIXOS
        ''}

        if [ -h /var/lib/containers/${ctr.name}/var/src/nixos-config ] && (! ping -c1 -q -w5 ${ctr.name}.r); then
          /run/current-system/sw/bin/nixos-container run ${ctr.name} -- nixos-rebuild -I /var/src switch
        else
          echo 'no nixos config, or target already online, bailing out'
          ${(stop ctr.name).${ctr.format}}
          /run/current-system/sw/bin/nixos-container stop ${ctr.name}
        fi
      '')
      (pkgs.writeDashBin "stop-${ctr.name}" ''
        set -euf

        /run/current-system/sw/bin/nixos-container stop ${ctr.name}
        ${(stop ctr.name).${ctr.format}}
      '')
    ]) cfg.containers);
  };
}
