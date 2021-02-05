with import <stockholm/lib>;
{ config, pkgs, ... }: let

  out = {
    options.krebs.setuid = api;
    config = mkIf (config.krebs.setuid != {}) imp;
  };

  api = mkOption {
    default = {};
    type = let
      # TODO make wrapperDir configurable
      inherit (config.security) wrapperDir;
      inherit (config.users) groups users;
    in types.attrsOf (types.submodule (self: let cfg = self.config; in {
      options = {
        name = mkOption {
          type = types.filename;
          default = cfg._module.args.name;
        };
        envp = mkOption {
          type = types.nullOr (types.attrsOf types.str);
          default = null;
        };
        filename = mkOption {
          type = mkOptionType {
            # TODO unyuck string and merge with toC
            name = "derivation or string";
            check = x:
              isDerivation x ||
              isString x;
          };
          apply = toString;
        };
        owner = mkOption {
          default = "root";
          type = types.enum (attrNames users);
        };
        group = mkOption {
          default = "root";
          type = types.enum (attrNames groups);
        };
        mode = mkOption {
          default = "4710";
          type = mkOptionType {
            # TODO admit symbolic mode
            name = "octal mode";
            check = test "[0-7][0-7][0-7][0-7]";
            merge = mergeOneOption;
          };
        };
        activate = mkOption {
          type = types.str;
          visible = false;
          readOnly = true;
        };
      };
      config.activate = let
        src = pkgs.exec cfg.name {
          inherit (cfg) envp filename;
        };
        dst = "${wrapperDir}/${cfg.name}";
      in ''
        cp ${src} ${dst}
        chown ${cfg.owner}.${cfg.group} ${dst}
        chmod ${cfg.mode} ${dst}
      '';
    }));
  };

  imp = {
    system.activationScripts."krebs.setuid" = stringAfter [ "wrappers" ]
      (concatMapStringsSep "\n" (getAttr "activate") (attrValues config.krebs.setuid));
  };

in out
