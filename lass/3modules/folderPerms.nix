{ config, lib, pkgs, ... }:

let
  inherit (pkgs)
    writeScript
  ;

  inherit (lib)
    concatMapStringsSep
    concatStringsSep
    mkEnableOption
    mkIf
    mkOption
    types
  ;

  cfg = config.lass.folderPerms;

  out = {
    options.lass.folderPerms = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "folder permissions";
    permissions = mkOption {
      type = with types; listOf (submodule ({
        options = {
          path = mkOption {
            type = str;
          };
          permission = mkOption {
            type = nullOr str;
            example = "755";
            description = ''
              basically anything that chmod takes as permission
            '';
            default = null;
          };
          owner = mkOption {
            type = nullOr str;
            example = "root:root";
            description = ''
              basically anything that chown takes as owner
            '';
            default = null;
          };
          recursive = mkOption {
            type = bool;
            default = false;
          };
        };
      }));
    };
  };

  imp = {
    systemd.services.lass-folderPerms = {
      description = "lass-folderPerms";
      wantedBy = [ "multi-user.target" ];

      path = with pkgs; [
        coreutils
      ];

      restartIfChanged = true;

      serviceConfig = {
        type = "simple";
        RemainAfterExit = true;
        Restart = "always";
        ExecStart = "@${startScript}";
      };
    };
  };

  startScript = writeScript "lass-folderPerms" ''
    ${concatMapStringsSep "\n" writeCommand cfg.permissions}
  '';

  writeCommand = fperm:
    concatStringsSep "\n" [
      (buildPermission fperm)
      (buildOwner fperm)
    ];

  buildPermission = perm:
    if (perm.permission == null) then
      ""
    else
      if perm.recursive then
        "chmod -R ${perm.permission} ${perm.path}"
      else
        "chmod ${perm.permission} ${perm.path}"
  ;

  buildOwner = perm:
    if (perm.owner == null) then
      ""
    else
      if perm.recursive then
        "chown -R ${perm.owner} ${perm.path}"
      else
        "chown ${perm.owner} ${perm.path}"
  ;

in out
