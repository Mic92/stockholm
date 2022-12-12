with import ./lib;
{ config, pkgs, ... }: let
  cfg = config.tv.slock;
in {
  options.tv.slock = {
    enable = mkEnableOption "tv.slock";
    package = mkOption {
      default = pkgs.writeDashBin "slock" ''
        set -efu
        display=''${DISPLAY#:}
        service=slock-$LOGNAME@$display.service
        exec ${pkgs.systemd}/bin/systemctl start "$service"
      '';
      type = types.package;
    };
    user = mkOption {
      type = types.user;
    };
  };
  config = mkIf cfg.enable {
    security.polkit.extraConfig = /* js */ ''
      polkit.addRule(function(action, subject) {
        if (action.id === "org.freedesktop.systemd1.manage-units" &&
            subject.user === ${toJSON cfg.user.name} &&
            /^slock-${cfg.user.name}@[0-9]+\.service$/.test(action.lookup("unit")) ) {
          return polkit.Result.YES;
        }
      });
    '';
    systemd.services."slock-${cfg.user.name}@" = {
      conflicts = [
        "picom@%i.target"
      ];
      environment = {
        DISPLAY = ":%I";
        LD_PRELOAD = pkgs.runCommandCC "slock-${cfg.user.name}.so" {
          passAsFile = ["text"];
          text = /* c */ ''
            #include <shadow.h>
            #include <unistd.h>

            static struct spwd entry = {
              .sp_namp = "",
              .sp_pwdp =
                ${toC config.users.users.${cfg.user.name}.hashedPassword},
              .sp_lstchg = 0,
              .sp_min = 0,
              .sp_max = 0,
              .sp_warn = 0,
              .sp_inact = 0,
              .sp_expire = 0,
              .sp_flag = 0,
            };

            extern struct spwd *getspnam(const char *name) { return &entry; }
            extern int setgroups(size_t size, const gid_t *list) { return 0; }
            extern int setgid(gid_t gid) { return 0; }
            extern int setuid(uid_t uid) { return 0; }
          '';
        } /* sh */ ''
          gcc -Wall -shared -o $out -xc "$textPath"
        '';
      };
      restartIfChanged = false;
      serviceConfig = {
        ExecStart = "${pkgs.slock}/bin/slock";
        ExecStopPost =
          "+${pkgs.systemd}/bin/systemctl start xsession@%i.target";
        OOMScoreAdjust = -1000;
        Restart = "on-failure";
        RestartSec = "100ms";
        StartLimitBurst = 0;
        SyslogIdentifier = "slock";
        User = cfg.user.name;
      };
    };
  };
}
