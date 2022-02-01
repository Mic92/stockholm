with import <stockholm/lib>;
{ config, pkgs, ... }: let
  cfg = config.krebs.sync-containers;
  paths = cname: {
    plain = "/var/lib/containers/${cname}/var/state";
    ecryptfs = "${cfg.dataLocation}/${cname}/ecryptfs";
    securefs = "${cfg.dataLocation}/${cname}/securefs";
  };
  start = cname: {
    plain = ''
      :
    '';
    ecryptfs = ''
      if ! mount | grep -q '${cfg.dataLocation}/${cname}/ecryptfs on /var/lib/containers/${cname}/var/state type ecryptfs'; then
        if [ -e ${cfg.dataLocation}/${cname}/ecryptfs/.cfg.json ]; then
          ${pkgs.ecrypt}/bin/ecrypt mount ${cfg.dataLocation}/${cname}/ecryptfs /var/lib/containers/${cname}/var/state
        else
          ${pkgs.ecrypt}/bin/ecrypt init ${cfg.dataLocation}/${cname}/ecryptfs /var/lib/containers/${cname}/var/state
        fi
      fi
    '';
    securefs = ''
      ## TODO init file systems if it does not exist
      # ${pkgs.securefs}/bin/securefs create --format 3 ${cfg.dataLocation}/${cname}/securefs
      if ! ${pkgs.mount}/bin/mount | grep -q '^securefs on /var/lib/containers/${cname}/var/state type fuse.securefs'; then
        ${pkgs.securefs}/bin/securefs mount ${cfg.dataLocation}/${cname}/securefs /var/lib/containers/${cname}/var/state -b -o allow_other -o default_permissions
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
  };
in {
  options.krebs.sync-containers = {
    dataLocation = mkOption {
      description = ''
        location where the encrypted sync-container lie around
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
          hostIp = mkOption { # TODO find this automatically
            description = ''
              hostAddress of the privateNetwork
            '';
            example = "10.233.2.15";
            type = types.str;
          };
          localIp = mkOption { # TODO find this automatically
            description = ''
              localAddress of the privateNetwork
            '';
            example = "10.233.2.16";
            type = types.str;
          };
          format = mkOption {
            description = ''
              file system encrption format of the container
            '';
            type = types.enum [ "plain" "ecryptfs" "securefs" ];
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

    krebs.permown = (mapAttrs' (_: ctr: nameValuePair "${(paths ctr.name).${ctr.format}}" ({
      file-mode = "u+rw";
      directory-mode = "u+rwx";
      owner = "syncthing";
      keepGoing = false;
    })) cfg.containers);

    systemd.services = mapAttrs' (n: ctr: nameValuePair "containers@${ctr.name}" ({
      reloadIfChanged = mkForce false;
    })) cfg.containers;

    containers = mapAttrs' (n: ctr: nameValuePair ctr.name ({
      config = { ... }: {
        environment.systemPackages = [
          pkgs.git
        ];
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
      hostAddress = ctr.hostIp;
      localAddress = ctr.localIp;
    })) cfg.containers;

    environment.systemPackages = flatten (mapAttrsToList (n: ctr: [
      (pkgs.writeDashBin "start-${ctr.name}" ''
        set -euf
        set -x

        mkdir -p /var/lib/containers/${ctr.name}/var/state

        ${(start ctr.name).${ctr.format}}

        STATE=$(${pkgs.nixos-container}/bin/nixos-container status ${ctr.name})
        if [ "$STATE" = 'down' ]; then
          ${pkgs.nixos-container}/bin/nixos-container start ${ctr.name}
        fi

        ${pkgs.nixos-container}/bin/nixos-container run ${ctr.name} -- ${pkgs.writeDash "deploy-${ctr.name}" ''
          set -x

          mkdir -p /var/state/var_src
          ln -sfTr /var/state/var_src /var/src
          touch /etc/NIXOS
        ''}

        if [ -h /var/lib/containers/${ctr.name}/var/src/nixos-config ] && (! ping -c1 -q -w5 ${ctr.name}.r); then
          ${pkgs.nixos-container}/bin/nixos-container run ${ctr.name} -- nixos-rebuild -I /var/src switch
        else
          ${(stop ctr.name).${ctr.format}}
        fi
      '')
      (pkgs.writeDashBin "stop-${ctr.name}" ''
        set -euf

        ${pkgs.nixos-container}/bin/nixos-container stop ${ctr.name}
        ${(stop ctr.name).${ctr.format}}
      '')
    ]) cfg.containers);
  };
}
