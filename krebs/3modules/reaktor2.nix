with import <stockholm/lib>;
{ config, pkgs, ... }: {

  options.krebs.reaktor2 = mkOption {
    default = {};
    type = types.attrsOf (types.submodule (self: let
      name = self.config._module.args.name;
    in {
      options = {
        nick = mkOption {
          default = name;
          # TODO types.irc.nickname
          type = types.str;
        };
        hostname = mkOption {
          default = "irc.r";
          type = types.hostname;
        };
        port = mkOption {
          default = "6667";
          # TODO type = types.service-name
        };
        plugins = mkOption {
          default = [];
          type = types.listOf types.attrs;
        };
        stateDir = mkOption {
          default = "/var/lib/${self.config.user}";
          readOnly = true;
          type = types.absolute-pathname;
        };
        systemd-service-name = mkOption {
          default = "reaktor2${optionalString (name != "default") "-${name}"}";
          type = types.filename;
        };
        user = mkOption {
          default = self.config.systemd-service-name;
          type = types.str;
        };
      };
    }));
  };

  config = {
    systemd.services = flip mapAttrs' config.krebs.reaktor2 (_: cfg:
      nameValuePair cfg.systemd-service-name {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = cfg.user;
          Group = "reaktor2";
          DynamicUser = true;
          StateDirectory = cfg.user;
          ExecStart = let
            configFile = pkgs.writeJSON configFileName configValue;
            configFileName = "${cfg.systemd-service-name}.config.json";
            configValue = recursiveUpdate {
              logTime = false;
            } (removeAttrs cfg ["_module"]);
          in "${pkgs.reaktor2}/bin/reaktor ${configFile}";
          Restart = "always";
          RestartSec = "30";
        };
      }
    );
  };
}
