with import ./lib;
{ config, pkgs, ... }: {
  options = {
    tv.lidControl.enable = mkEnableOption "tv.lidControl";
  };
  config = let
    cfg = config.tv.lidControl;
  in mkIf cfg.enable {
    services.acpid.enable = true;
    services.acpid.lidEventCommands = /* sh */ ''
      set -- $1

      # usage: vt_is_xserver NUMBER
      vt_is_xserver() {
        ${pkgs.iproute}/bin/ss -lp src unix:/tmp/.X11-unix/X* |
        ${pkgs.gnused}/bin/sed -n 's|.*/tmp/.X11-unix/X\([0-9]\+\)\>.*|\1|p' |
        ${pkgs.gnugrep}/bin/grep -Fqx "$1"
      }

      console=$(${pkgs.kbd}/bin/fgconsole)

      if vt_is_xserver "$console"; then
        # usage: run_on_display COMMAND [ARG...]
        run_on_display() {
          owner=$(${pkgs.coreutils}/bin/stat -c %u /tmp/.X11-unix/X$console)
          ${pkgs.systemd}/bin/systemd-run -GPq \
              -E DISPLAY=:$console \
              --uid=$owner \
              "$@"
        }
        case $3 in
          open)
            run_on_display ${pkgs.xorg.xset}/bin/xset dpms force on
            ;;
          close)
            run_on_display ${pkgs.xorg.xset}/bin/xset dpms force off
            ;;
        esac
      fi
    '';
    services.logind.lidSwitch = "ignore";
    services.logind.lidSwitchDocked = "ignore";
    services.logind.lidSwitchExternalPower = "ignore";
  };
}
