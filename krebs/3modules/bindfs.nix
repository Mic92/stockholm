{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.krebs.bindfs;
in {
  options.krebs.bindfs = mkOption {
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        target = mkOption {
          description = ''
            destination where bindfs mounts to.
            second positional argument to bindfs.
          '';
          default = config._module.args.name;
          type = types.absolute-pathname;
        };
        source = mkOption {
          description = ''
            source folder where the mounted directory is originally.
            first positional argument to bindfs.
          '';
          type = types.absolute-pathname;
        };
        options = mkOption {
          description = ''
            additional arguments to bindfs
          '';
          type = types.listOf types.str;
          default = [];
        };
        clearTarget = mkOption {
          description = ''
            whether to clear the target folder before mounting
          '';
          type = types.bool;
          default = false;
        };
      };
    }));
    default = {};
  };

  config = mkIf (cfg != {}) {
    systemd.services = mapAttrs' (n: mount: let
      name = replaceStrings [ "/" ] [ "_" ] n;
    in nameValuePair "bindfs-${name}" {
      wantedBy = [ "local-fs.target" ];
      path = [ pkgs.coreutils ];
      serviceConfig = {
        ExecStartPre = pkgs.writeDash "bindfs-init-${name}" ''
          ${optionalString mount.clearTarget ''
            rm -rf '${mount.target}'
          ''}
          mkdir -p '${mount.source}'
          mkdir -p '${mount.target}'
        '';
        ExecStart = "${pkgs.bindfs}/bin/bindfs -f ${concatStringsSep " " mount.options} ${mount.source} ${mount.target}";
      };
    }) cfg;
  };
}
