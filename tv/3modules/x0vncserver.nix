with import ./lib;
{ config, pkgs, ... }: let
  cfg = config.tv.x0vncserver;
in {
  options.tv.x0vncserver = {
    display = mkOption {
      default = ":${toString config.services.xserver.display}";
      type = types.str;
    };
    enable = mkEnableOption "tv.x0vncserver";
    pwfile = mkOption {
      default = toString <secrets> + "/vncpasswd";
      description = ''
        Use vncpasswd to edit pwfile.
        See: nix-shell -p tigervnc --run 'man vncpasswd'
      '';
      type = types.absolute-pathname;
    };
    rfbport = mkOption {
      default = 5900;
      type = types.int;
    };
    user = mkOption {
      default = config.krebs.build.user;
      type = types.user;
    };
  };
  config = mkIf cfg.enable {
    krebs.systemd.services.x0vncserver = {};
    systemd.services.x0vncserver = {
      after = [ "graphical.target" ];
      requires = [ "graphical.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.tigervnc}/bin/x0vncserver ${toString [
          "-display ${cfg.display}"
          "-passwordfile \${CREDENTIALS_DIRECTORY}/pwfile"
          "-rfbport ${toString cfg.rfbport}"
        ]}";
        LoadCredential = "ssh_key:${cfg.pwfile}";
        User = cfg.user.name;
      };
    };
    tv.iptables.input-retiolum-accept-tcp = singleton (toString cfg.rfbport);
  };
}
