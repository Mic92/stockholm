{ config, lib, pkgs, ... }:

#TODO:
#prefix with Attribute Name
#ex: urxvt

#TODO: make users configureable
#
#we need something like this:
#
#a.u = [1 2];
#a.x = "test";

#b.u = [1];
#b.x = "tast";

#    |
#    v

#1."test\ntast";
#2."test";
#
#
#users = mkOption {
#  type = types.str;
#  default = ;
#  description = ''
#    users for this xresources config.
#  '';
#};
with builtins;
with lib;


let

  inherit (import ../../lib { inherit pkgs; }) shell-escape;
  inherit (pkgs) writeScript;

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
      user = cfg.user;
      xres = concatStringsSep "\n" (attrValues cfg.resources);

    in mkIf cfg.enable {
        services.xserver.displayManager.sessionCommands = ''
          echo ${shell-escape xres} | xrdb -merge
        '';
      };
      

}
