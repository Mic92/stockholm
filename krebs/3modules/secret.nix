{ config, lib, pkgs, ... }:
with import ../../lib/pure.nix { inherit lib; }; let
  cfg = config.krebs.secret;
in {
  options.krebs.secret = {
    directory = mkOption {
      default = toString <secrets>;
      type = types.absolute-pathname;
    };
    files = mkOption {
      type = with pkgs.stockholm.lib.types; attrsOf secret-file;
      default = {};
      apply = mapAttrs (name: secret-file:
        if types.absolute-pathname.check secret-file.source-path then
          secret-file
        else
          secret-file // {
            source-path = "${config.krebs.secret.directory}/secret-file.source-path";
          }
      );
    };
  };
  config = lib.mkIf (cfg.files != {}) {
    systemd.paths =
      mapAttrs'
        (name: file: nameValuePair "secret-trigger-${systemd.encodeName name}" {
          wantedBy = ["multi-user.target"];
          pathConfig.PathChanged = file.source-path;
        })
        cfg.files;
    systemd.services =
      mapAttrs'
        (name: file: nameValuePair "secret-trigger-${systemd.encodeName name}" {
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkgs.systemd}/bin/systemctl restart ${shell.escape file.service}";
          };
        })
        cfg.files
      //
      mapAttrs'
        (name: file: nameValuePair "secret-${systemd.encodeName name}" {
          wantedBy = ["multi-user.target"];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = "yes";
            ExecStart = toString [
              "${pkgs.coreutils}/bin/install"
              "-D"
              "--compare"
              "--verbose"
              "--mode=${file.mode}"
              "--owner=${file.owner.name}"
              "--group=${file.group-name}"
              file.source-path
              file.path
            ];
          };
        })
        cfg.files;
  };
}
