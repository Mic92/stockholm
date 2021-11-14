{ config, pkgs, lib, ... }:

with import <stockholm/lib>;
{
  options.lass.xjail = mkOption {
    type = types.attrsOf (types.submodule ({ config, ...}: {
      options = {
        name = mkOption {
          type = types.str;
          default = config._module.args.name;
        };
        user = mkOption {
          type = types.str;
          default = config.name;
        };
        groups = mkOption {
          type = types.listOf types.str;
          default = [];
        };
        from = mkOption {
          type = types.str;
          default = "lass";
        };
        display = mkOption {
          type = types.str;
          default = toString (genid_uint31 config._module.args.name);
        };
        dpi = mkOption {
          type = types.int;
          default = 90;
        };
        extraXephyrArgs = mkOption {
          type = types.str;
          default = "";
        };
        extraVglrunArgs = mkOption {
          type = types.str;
          default = "";
        };
        script = mkOption {
          type = types.path;
          default = pkgs.writeScript "echo_lol" "echo lol";
        };
        vglrun = mkOption {
          type = types.bool;
          default = false;
        };
        wm = mkOption {
          #TODO find type
          type = types.str;
          defaultText = "‹script›";
          default = "${pkgs.writeHaskellPackage "xephyrify-xmonad" {
            executables.xmonad = {
              extra-depends = [
                "containers"
                "unix"
                "xmonad"
              ];
              text = /* haskell */ ''
                module Main where
                import XMonad
                import Data.Monoid
                import System.Posix.Process (executeFile)
                import qualified Data.Map as Map

                main :: IO ()
                main = do
                  xmonad def
                    { workspaces = [ "1" ]
                    , layoutHook = myLayoutHook
                    , keys = myKeys
                    , normalBorderColor  = "#000000"
                    , focusedBorderColor = "#000000"
                    , handleEventHook = myEventHook
                    }

                myEventHook :: Event -> X All

                myEventHook (ConfigureEvent { ev_event_type = 22 }) = do
                  spawn "${pkgs.xorg.xrandr}/bin/xrandr >/dev/null 2>&1"
                  return (All True)

                myEventHook _ = do
                  return (All True)

                myLayoutHook = Full
                myKeys _ = Map.fromList []
              '';
            };
          }}/bin/xmonad";
        };
      };
    }));
    default = {};
  };

  options.lass.xjail-bins = mkOption {
    type = types.attrsOf types.path;
  };

  # implementation
  config = let
    scripts = mapAttrs' (name: cfg:
      let
        newOrExisting = pkgs.writeDash "${cfg.name}-existing" ''
          DISPLAY=:${cfg.display} ${pkgs.xorg.xrandr}/bin/xrandr
          if test $? -eq 0; then
            echo using existing xephyr
            ${sudo_} "$@"
          else
            echo starting new xephyr
            ${xephyr_} "$@"
          fi
        '';
        xephyr_ = pkgs.writeDash "${cfg.name}-xephyr" ''
          ${pkgs.xorg.xorgserver}/bin/Xephyr -br -ac -reset -terminate -resizeable -nolisten local -dpi ${toString cfg.dpi} ${cfg.extraXephyrArgs} :${cfg.display} &
          XEPHYR_PID=$!
          DISPLAY=:${cfg.display} ${cfg.wm} &
          WM_PID=$!
          ${sudo_} "$@"
          ${pkgs.coreutils}/bin/kill $WM_PID
          ${pkgs.coreutils}/bin/kill $XEPHYR_PID
        '';
        # TODO fix xephyr which doesn't honor resizes anymore
        sudo_ = pkgs.writeDash "${cfg.name}-sudo" (if cfg.vglrun then ''
          /var/run/wrappers/bin/sudo -u ${cfg.name} -i ${vglrun_} "$@"
        '' else ''
          #/var/run/wrappers/bin/sudo -u ${cfg.name} -i env DISPLAY=:${cfg.display} ${cfg.script} "$@"
          /var/run/wrappers/bin/sudo -u ${cfg.name} -i ${cfg.script} "$@"

        '');
        vglrun_ = pkgs.writeDash "${cfg.name}-vglrun" ''
          DISPLAY=:${cfg.display} ${pkgs.virtualgl}/bin/vglrun ${cfg.extraVglrunArgs} ${cfg.script} "$@"
        '';
      in nameValuePair name {
        existing = newOrExisting;
        xephyr = xephyr_;
        sudo = sudo_;
        vglrun = vglrun_;
      }
    ) config.lass.xjail;
  in {

    users.users = mapAttrs' (_: cfg:
      nameValuePair cfg.name {
        uid = genid_uint31 cfg.name;
        home = "/home/${cfg.name}";
        useDefaultShell = true;
        createHome = true;
        extraGroups = cfg.groups;
        isNormalUser = true;
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
      nameValuePair name (pkgs.writeScriptBin cfg.name ''
        ${scripts.${name}.sudo} "$@"
      '')
    ) config.lass.xjail;
  };
}
