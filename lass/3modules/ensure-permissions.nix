{ config, pkgs, ... }: with import <stockholm/lib>;

let

  cfg = config.lass.ensure-permissions;

in

{
  options.lass.ensure-permissions = mkOption {
    default = [];
    type = types.listOf (types.submodule ({
      options = {

        folder = mkOption {
          type = types.absolute-pathname;
        };

        owner = mkOption {
          # TODO user type
          type = types.str;
          default = "root";
        };

        group = mkOption {
          # TODO group type
          type = types.str;
          default = "root";
        };

        permission = mkOption {
          # TODO permission type
          type = types.str;
          default = "u+rw,g+rw";
        };

      };
    }));
  };

  config = mkIf (cfg != []) {

  system.activationScripts.ensure-permissions = concatMapStringsSep "\n" (plan: ''
    ${pkgs.coreutils}/bin/mkdir -p ${plan.folder}
    ${pkgs.coreutils}/bin/chmod -R ${plan.permission} ${plan.folder}
    ${pkgs.coreutils}/bin/chown -R ${plan.owner}:${plan.group} ${plan.folder}
  '') cfg;
    systemd.services =
      listToAttrs (map (plan: nameValuePair "ensure-permisson.${replaceStrings ["/"] ["_"] plan.folder}" {
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Restart = "always";
          RestartSec = 10;
          ExecStart = pkgs.writeDash "ensure-perms" ''
            ${pkgs.inotifyTools}/bin/inotifywait -mrq -e CREATE --format %w%f ${plan.folder} \
              | while IFS= read -r FILE; do
                ${pkgs.coreutils}/bin/chmod -R ${plan.permission} "$FILE" 2>/dev/null
                ${pkgs.coreutils}/bin/chown -R ${plan.owner}:${plan.group} "$FILE" 2>/dev/null
              done
          '';
        };
      }) cfg)
    ;

  };
}
