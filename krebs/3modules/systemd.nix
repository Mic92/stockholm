{ config, pkgs, ... }: let {
  lib = import ../../lib;

  body.options.krebs.systemd.services = lib.mkOption {
    default = {};
    type = lib.types.attrsOf (lib.types.submodule (cfg_: let
      serviceName = cfg_.config._module.args.name;
      cfg = config.systemd.services.${serviceName} // cfg_.config;
    in {
      options = {
        credentialPaths = lib.mkOption {
          default =
            lib.sort
              lib.lessThan
              (lib.filter
                lib.types.absolute-pathname.check
                (map
                  (lib.compose [ lib.maybeHead (lib.match "[^:]*:(.*)") ])
                  (lib.toList cfg.serviceConfig.LoadCredential)));
          readOnly = true;
        };
        credentialUnitName = lib.mkOption {
          default = "trigger-${lib.systemd.encodeName serviceName}";
          readOnly = true;
        };
        restartIfCredentialsChange = lib.mkOption {
          default = false;
          description = ''
            Whether to restart the service whenever any of its credentials
            change.  Only credentials with an absolute path in LoadCredential=
            are supported.
          '';
          type = lib.types.bool;
        };
      };
    }));
  };

  body.config.systemd = lib.mkMerge (lib.mapAttrsToList (serviceName: cfg: {
    paths.${cfg.credentialUnitName} = {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathChanged = cfg.credentialPaths;
    };
    services.${cfg.credentialUnitName} = {
      serviceConfig = {
        Type = "oneshot";
        StateDirectory = "credentials";
        ExecStart = pkgs.writeDash "${cfg.credentialUnitName}.sh" ''
          set -efu

          PATH=${lib.makeBinPath [
            pkgs.coreutils
            pkgs.diffutils
            pkgs.systemd
          ]}

          cache=/var/lib/credentials/${lib.shell.escape serviceName}.sha1sum
          tmpfile=$(mktemp -t "$(basename "$cache")".XXXXXXXX)
          trap 'rm -f "$tmpfile"' EXIT

          sha1sum ${toString cfg.credentialPaths} > "$tmpfile"
          if test -f "$cache" && cmp -s "$tmpfile" "$cache"; then
            exit
          fi
          mv "$tmpfile" "$cache"

          systemctl restart ${lib.shell.escape serviceName}
        '';
      };
    };
  }) config.krebs.systemd.services);
}
