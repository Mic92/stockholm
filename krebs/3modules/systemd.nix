{ config, options, pkgs, ... }: let {
  lib = import ../../lib;

  body.options.krebs.systemd.services = lib.mkOption {
    default = {};
    type = lib.types.attrs;
    description = ''
      Definition of systemd service units with bonus features.

      Services defined using this option will be restarted whenever any file
      (described by an absolute path) used in LoadCredential changes.
    '';
  };

  body.config.systemd =
    lib.mkMerge
      (lib.flatten
        (lib.mapAttrsToList (serviceName: cfg: let
          prefix = [ "krebs" "systemd" "services" serviceName ];
          opts = options.systemd.services.type.getSubOptions prefix;

          paths =
            lib.filter
              lib.types.absolute-pathname.check
              (map
                (lib.compose [ lib.maybeHead (lib.match "[^:]*:(.*)") ])
                (cfg.serviceConfig.LoadCredential or []));
        in
          lib.singleton {
            services.${serviceName} = cfg;
          }
          ++
          lib.optionals (cfg.enable or opts.enable.default) (map (path: let
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
                  "${pkgs.systemd}/bin/systemctl restart"
                  (lib.shell.escape serviceName)
                ]);
              };
            };
          }) paths)
        ) config.krebs.systemd.services));
}
