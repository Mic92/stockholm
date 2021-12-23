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
        serviceConfig.LoadCredential = lib.mkOption {
          apply = lib.toList;
          type =
            lib.types.either lib.types.str (lib.types.listOf lib.types.str);
        };
      };
    });
  };

  body.config.systemd =
    lib.mkMerge
      (lib.flatten
        (lib.mapAttrsToList (serviceName: cfg: let
          paths =
            lib.filter
              lib.types.absolute-pathname.check
              (map
                (lib.compose [ lib.maybeHead (lib.match "[^:]*:(.*)") ])
                cfg.serviceConfig.LoadCredential);
        in
          lib.singleton {
            services.${serviceName} = {
              serviceConfig = {
                LoadCredential = cfg.serviceConfig.LoadCredential;
              };
            };
          }
          ++
          lib.optionals (cfg.ifCredentialsChange != null) (map (path: let
            triggerName = "trigger-${lib.systemd.encodeName path}";
          in {
            paths.${triggerName} = {
              wantedBy = ["multi-user.target"];
              pathConfig.PathChanged = path;
            };
            services.${triggerName} = {
              serviceConfig = {
                Type = "oneshot";
                ExecStart = lib.singleton (toString [
                  "${pkgs.systemd}/bin/systemctl ${cfg.ifCredentialsChange}"
                  (lib.shell.escape serviceName)
                ]);
              };
            };
          }) paths)
        ) config.krebs.systemd.services));
}
