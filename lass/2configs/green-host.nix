{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

let

  cname = "green";
  cryfs = pkgs.cryfs.overrideAttrs (old: {
    patches = [
      (pkgs.writeText "file_mode.patch" ''
        --- a/src/cryfs/filesystem/CryNode.cpp
        +++ b/src/cryfs/filesystem/CryNode.cpp
        @@ -171,7 +171,7 @@ CryNode::stat_info CryNode::stat() const {
             result.uid = fspp::uid_t(getuid());
             result.gid = fspp::gid_t(getgid());
         #endif
        -    result.mode = fspp::mode_t().addDirFlag().addUserReadFlag().addUserWriteFlag().addUserExecFlag();
        +    result.mode = fspp::mode_t().addDirFlag().addUserReadFlag().addUserWriteFlag().addUserExecFlag().addGroupReadFlag().addGroupExecFlag().addOtherReadFlag().addOtherExecFlag();;
             result.size = fsblobstore::DirBlob::DIR_LSTAT_SIZE;
             //TODO If possible without performance loss, then for a directory, st_nlink should return number of dir entries (including "." and "..")
             result.nlink = 1;
      '')
    ] ++ old.patches;
  });

in {
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];

  programs.fuse.userAllowOther = true;

  services.syncthing.declarative.folders."/var/lib/sync-containers/${cname}".devices = [ "icarus" "skynet" "littleT" "shodan" ];
  # krebs.permown."/var/lib/sync-containers/${cname}" = {
  #   owner = "root";
  #   group = "syncthing";
  #   umask = "0007";
  # };

  systemd.services."container@green".reloadIfChanged = mkForce false;
  containers.${cname} = {
    config = { ... }: {
      environment.systemPackages = [
        pkgs.git
        pkgs.rxvt_unicode.terminfo
      ];
      services.openssh.enable = true;
      users.users.root.openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
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
    hostAddress = "10.233.2.15"; # TODO find way to automatically calculate IPs
    localAddress = "10.233.2.16"; # TODO find way to automatically calculate IPs
  };

  environment.systemPackages = [
    (pkgs.writeDashBin "start-${cname}" ''
      set -euf

      mkdir -p /var/lib/containers/${cname}/var/state
      chown ${config.services.syncthing.user}: /var/lib/containers/${cname}/var/state
      if ! ${pkgs.mount}/bin/mount | grep -q '^cryfs@/var/lib/sync-containers/${cname} on /var/lib/containers/${cname}/var/state '; then
        /run/wrappers/bin/sudo -u "${config.services.syncthing.user}" \
          ${cryfs}/bin/cryfs /var/lib/sync-containers/${cname} /var/lib/containers/${cname}/var/state -o allow_other -o default_permissions
      fi

      STATE=$(${pkgs.nixos-container}/bin/nixos-container status ${cname})
      if [ "$STATE" = 'down' ]; then
        ${pkgs.nixos-container}/bin/nixos-container start ${cname}
      fi

      if ! ping -c1 -q -w5 ${cname}.r && [ -d /var/lib/containers/${cname}/var/src ]; then
        ${pkgs.nixos-container}/bin/nixos-container run ${cname} -- ${pkgs.writeDash "deploy-${cname}" ''
          mkdir -p /var/state/var_src
          ln -sf state/var_Src /var/src
          nixos-rebuild -I /var/src switch
        ''}
      fi
    '')
    (pkgs.writeDashBin "stop-${cname}" ''
      set -euf

      ${pkgs.nixos-container}/bin/nixos-container stop ${cname}
      ${cryfs}/bin/cryfs-unmount /var/lib/containers/${cname}/var/state
    '')
  ];
}
