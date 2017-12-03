{ config, lib, pkgs, ... }:

#TODO:
#prefix with Attribute Name
#ex: urxvt

with builtins;
with lib;


let

  inherit (pkgs) writeScript writeText;

in

{

  options = {
    services.xresources.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable the automatic loading of Xresources definitions at display-manager start;
      '';
    };

    services.xresources.resources = mkOption {
      default = {};
      type = types.attrsOf types.str;
      example = {
        urxvt = ''
          URxvt*scrollBar: false
          URxvt*urgentOnBell: true
        '';
      };
      description = ''
        Xresources definitions.
      '';
    };
  };

  config =
    let
      cfg = config.services.xresources;
      xres = writeText "xresources" (concatStringsSep "\n" (attrValues cfg.resources));

    in mkIf cfg.enable {
        services.xserver.displayManager.sessionCommands = ''
          ${pkgs.xorg.xrdb}/bin/xrdb -merge ${xres}
        '';
      };
}
