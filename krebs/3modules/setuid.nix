{ config, pkgs, lib, ... }:
with import ../../lib/pure.nix { inherit lib; };
let

  out = {
    options.krebs.setuid = api;
    config = mkIf (config.krebs.setuid != {}) imp;
  };

  api = mkOption {
    default = {};
    type = let
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
        capabilities = mkOption {
          default = [];
          type = types.listOf types.str;
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
        wrapperDir = mkOption {
          default = config.security.wrapperDir;
          type = types.absolute-pathname;
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
        dst = "${cfg.wrapperDir}/${cfg.name}";
      in /* sh */ ''
        mkdir -p ${cfg.wrapperDir}
        cp ${src} ${dst}
        chown ${cfg.owner}:${cfg.group} ${dst}
        chmod ${cfg.mode} ${dst}
        ${optionalString (cfg.capabilities != []) /* sh */ ''
          ${pkgs.libcap.out}/bin/setcap ${concatMapStringsSep "," shell.escape cfg.capabilities} ${dst}
        ''}
      '';
    }));
  };

  imp = {
    systemd.services."krebs.setuid" = {
      wantedBy = [ "suid-sgid-wrappers.service" ];
      after = [ "suid-sgid-wrappers.service" ];
      path = [
        pkgs.coreutils
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeDash "krebs.setuid.sh" ''
          ${concatMapStringsSep "\n"
            (getAttr "activate")
            (attrValues config.krebs.setuid)
          }
        '';
      };
      unitConfig = {
        DefaultDependencies = false;
      };
    };
  };

in out
