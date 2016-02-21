{ config, lib, pkgs, ... }@args: with config.krebs.lib; let
  cfg = config.krebs.secret;
in {
  options.krebs.secret = {
    files = mkOption {
      type = with types; attrsOf secret-file;
      default = {};
    };
  };
  config = lib.mkIf (cfg.files != {}) {
    systemd.services.secret = let
      # TODO fail if two files have the same path but differ otherwise
      files = unique (map (flip removeAttrs ["_module"])
                          (attrValues cfg.files));
    in {
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        SyslogIdentifier = "secret";
        ExecStart = pkgs.writeDash "install-secret-files" ''
          exit_code=0
          ${concatMapStringsSep "\n" (file: ''
            ${pkgs.coreutils}/bin/install \
                  -D \
                  --compare \
                  --verbose \
                  --mode=${shell.escape file.mode} \
                  --owner=${shell.escape file.owner-name} \
                  --group=${shell.escape file.group-name} \
                  ${shell.escape file.source-path} \
                  ${shell.escape file.path} \
                || exit_code=1
          '') files}
          exit $exit_code
        '';
      };
    };
  };
}
