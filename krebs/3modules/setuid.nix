{ config, pkgs, lib, ... }:
with import <stockholm/lib>;
let
  cfg = config.krebs.setuid;

  out = {
    options.krebs.setuid = api;
    config = imp;
  };

  api = mkOption {
    default = {};
    type = let
      # TODO make wrapperDir configurable
      inherit (config.security) wrapperDir;
      inherit (config.users) groups users;
    in types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        name = mkOption {
          type = types.filename;
          default = config._module.args.name;
        };
        envp = mkOption {
          type = types.attrsOf types.str;
          default = {};
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
            check = x:
              isString x &&
              match "[0-7][0-7][0-7][0-7]" x != null;
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
        src = pkgs.execve config.name {
          inherit (config) envp filename;
        };
        dst = "${wrapperDir}/${config.name}";
      in ''
        cp ${src} ${dst}
        chown ${config.owner}.${config.group} ${dst}
        chmod ${config.mode} ${dst}
      '';
    }));
  };

  imp = {
    system.activationScripts."krebs.setuid" = stringAfter [ "setuid" ]
      (concatMapStringsSep "\n" (getAttr "activate") (attrValues cfg));
  };

in out
