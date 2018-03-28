{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  options.lass.xjail = mkOption {
    type = types.attrsOf (types.submodule ({ config, ...}: {
      options = {
        user = mkOption {
          type = types.string;
          default = "nobody";
        };
        groups = mkOption {
          type = types.listOf types.str;
          default = [];
        };
        name = mkOption {
          type = types.string;
          default = config._module.args.name;
        };
        display = mkOption {
          type = types.string;
          default = toString (genid_signed config._module.args.name);
        };
        script = mkOption {
          type = types.path;
          default = pkgs.writeScript "echo_lol" "echo lol";
        };
        from = mkOption {
          type = types.string;
          default = "lass";
        };
      };
    }));
    default = {};
  };

  options.lass.xjail-bins = mkOption {
    type = types.attrsOf types.path;
  };

  # implementation
  config = {

    users.users = mapAttrs' (_: cfg:
      nameValuePair cfg.name {
        uid = genid cfg.name;
        home = "/home/${cfg.name}";
        useDefaultShell = true;
        createHome = true;
        extraGroups = cfg.groups;
      }
    ) config.lass.xjail;

    users.groups = mapAttrs' (_: cfg:
      nameValuePair cfg.name {
        members = [
          cfg.name
          cfg.from
        ];
      }
    ) config.lass.xjail;

    security.sudo.extraConfig = (concatStringsSep "\n" (mapAttrsToList (_: cfg:
      # TODO allow just the right script with sudo
      "${cfg.from} ALL=(${cfg.name}) NOPASSWD: ALL"
    ) config.lass.xjail));

    lass.xjail-bins = mapAttrs' (name: cfg:
      let
        sudo-wrapper = pkgs.writeScript name ''
          /var/run/wrappers/bin/sudo -u ${cfg.name} -i ${cfg.script} "$@"
        '';
      in nameValuePair name (pkgs.writeScriptBin cfg.name ''
        export NDISPLAY=${cfg.display}
        DISPLAY=:$NDISPLAY ${pkgs.xorg.xrandr}/bin/xrandr
        if test $? -eq 0; then
          echo xephyr already running
          export DISPLAY=:$NDISPLAY
          ${sudo-wrapper} "$@"
        else
          echo xephyr not running
          DROP_TO_USER=${cfg.name} ${pkgs.xephyrify}/bin/xephyrify ${sudo-wrapper} "$@"
        fi
      '')
    ) config.lass.xjail;
  };
}
