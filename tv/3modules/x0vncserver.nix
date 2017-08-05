with import <stockholm/lib>;
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
      default = {
        owner = cfg.user;
        path = "${cfg.user.home}/.vncpasswd";
        source-path = toString <secrets> + "/vncpasswd";
      };
      description = ''
        Use vncpasswd to edit pwfile.
        See: nix-shell -p tigervnc --run 'man vncpasswd'
      '';
      type = types.secret-file;
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
    krebs.secret.files = {
      x0vncserver-pwfile = cfg.pwfile;
    };
    systemd.services.x0vncserver = {
      after = [ "graphical.target" "secret.service" ];
      requires = [ "graphical.target" "secret.service" ];
      serviceConfig = {
        ExecStart = "${pkgs.tigervnc}/bin/x0vncserver ${toString [
          "-display ${cfg.display}"
          "-passwordfile ${cfg.pwfile.path}"
          "-rfbport ${toString cfg.rfbport}"
        ]}";
        User = cfg.user.name;
      };
    };
    tv.iptables.input-retiolum-accept-tcp = singleton (toString cfg.rfbport);
  };
}
