with import <stockholm/lib>;
{ config, pkgs, ... }: {

  options.krebs.permown = mkOption {
    default = {};
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        directory-mode = mkOption {
          default = "=rwx";
          type = types.str; # TODO
        };
        file-mode = mkOption {
          default = "=rw";
          type = types.str; # TODO
        };
        group = mkOption {
          apply = x: if x == null then "" else x;
          default = null;
          type = types.nullOr types.groupname;
        };
        owner = mkOption {
          type = types.username;
        };
        path = mkOption {
          default = config._module.args.name;
          type = types.absolute-pathname;
        };
        umask = mkOption {
          default = "0027";
          type = types.file-mode;
        };
      };
    }));
  };

  config = let
    plans = attrValues config.krebs.permown;
  in mkIf (plans != []) {

    system.activationScripts.permown = let
      mkdir = plan: /* sh */ ''
        ${pkgs.coreutils}/bin/mkdir -p ${shell.escape plan.path}
      '';
    in concatMapStrings mkdir plans;

    systemd.services = genAttrs' plans (plan: {
      name = "permown.${replaceStrings ["/"] ["_"] plan.path}";
      value = {
        environment = {
          DIR_MODE = plan.directory-mode;
          FILE_MODE = plan.file-mode;
          OWNER_GROUP = "${plan.owner}:${plan.group}";
          ROOT_PATH = plan.path;
        };
        path = [
          pkgs.coreutils
          pkgs.findutils
          pkgs.inotifyTools
        ];
        serviceConfig = {
          ExecStart = pkgs.writeDash "permown" ''
            set -efu

            find "$ROOT_PATH" -exec chown "$OWNER_GROUP" {} +
            find "$ROOT_PATH" -type d -exec chmod "$DIR_MODE" {} +
            find "$ROOT_PATH" -type f -exec chmod "$FILE_MODE" {} +

            inotifywait -mrq -e CREATE --format %w%f "$ROOT_PATH" |
            while read -r path; do
              if test -d "$path"; then
                exec "$0" "$@"
              fi
              chown "$OWNER_GROUP" "$path"
              chmod "$FILE_MODE" "$path"
            done
          '';
          Restart = "always";
          RestartSec = 10;
          UMask = plan.umask;
        };
        wantedBy = [ "multi-user.target" ];
      };
    });

  };

}
