with import ./lib;
{ config, pkgs, ... }: let
  cfg = {
    enable = config.services.xserver.enable && config.tv.Xresources != {};
    user = config.krebs.build.user;
  };
in {
  options.tv.Xresources = mkOption {
    default = {};
    type = types.attrsOf types.str;
  };
  config = {
    nixpkgs.overlays = singleton (self: super: {
      tv = super.tv or {} // {
        Xresources =
          self.writeText "Xresources"
            (concatStrings (mapAttrsToList (name: value: /* xdefaults */ ''
              ${name}: ${value}
            '') config.tv.Xresources));
      };
    });
    systemd.services.${if cfg.enable then "Xresources" else null} = {
      wantedBy = [ "graphical.target" ];
      after = [ "xmonad.service" ];
      environment = {
        DISPLAY = ":${toString config.services.xserver.display}";
      };
      serviceConfig = {
        ExecStart = "${pkgs.xorg.xrdb}/bin/xrdb ${pkgs.tv.Xresources}";
        RemainAfterExit = true;
        SyslogIdentifier = "Xresources";
        Type = "oneshot";
        User = cfg.user.name;
        WorkingDirectory = cfg.user.home;
      };
    };
  };
}
