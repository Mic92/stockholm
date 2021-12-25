{ config, pkgs, ... }: let {
  lib = import ../../lib;

  body.options.krebs.systemd.services = lib.mkOption {
    default = {};
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        ifCredentialsChange = lib.mkOption {
          default = "restart";
          description = ''
            Whether to reload or restart the service whenever any its
            credentials change.  Only credentials with an absolute path in
            LoadCredential= are supported.
          '';
          type = lib.types.enum [
            "reload"
            "restart"
            null
          ];
        };
      };
    });
  };

  body.config = {
    systemd.paths = lib.mapAttrs' (serviceName: _:
      lib.nameValuePair "trigger-${lib.systemd.encodeName serviceName}" {
        wantedBy = [ "multi-user.target" ];
        pathConfig.PathChanged =
          lib.filter
            lib.types.absolute-pathname.check
            (map
              (lib.compose [ lib.maybeHead (lib.match "[^:]*:(.*)") ])
              (lib.toList
                config.systemd.services.${serviceName}.serviceConfig.LoadCredential));
      }
    ) config.krebs.systemd.services;

    systemd.services = lib.mapAttrs' (serviceName: cfg:
      lib.nameValuePair "trigger-${lib.systemd.encodeName serviceName}" {
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.systemd}/bin/systemctl ${cfg.ifCredentialsChange} ${lib.shell.escape serviceName}";
        };
      }
    ) config.krebs.systemd.services;
  };
}
