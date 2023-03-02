{ config, lib, pkgs, ... }:
{
  systemd.services.antimicrox = {
    wantedBy = [ "multi-user.target" ];
    environment = {
      DISPLAY = ":0";
    };
    serviceConfig = {
      User = config.users.users.mainUser.name;
      ExecStartPre = lib.singleton (pkgs.writeDash "init_state" "echo 0 > /tmp/gamepad.state");
      ExecStart = "${pkgs.antimicrox}/bin/antimicrox --no-tray --hidden --profile ${./mouse.amgp}";
    };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "gamepad_mouse_disable" ''
      echo 1 > /tmp/gamepad.state
      ${pkgs.antimicrox}/bin/antimicrox --profile ${./empty.amgp}
    '')
    (pkgs.writers.writeDashBin "gamepad_mouse_enable" ''
      echo 0 > /tmp/gamepad.state
      ${pkgs.antimicrox}/bin/antimicrox --profile ${./mouse.amgp}
    '')
    (pkgs.writers.writeDashBin "gamepad_mouse_toggle" ''
      state=$(${pkgs.coreutils}/bin/cat /tmp/gamepad.state)
      if [ "$state" = 1 ]; then
        /run/current-system/sw/bin/gamepad_mouse_enable
      else
        /run/current-system/sw/bin/gamepad_mouse_disable
      fi
    '')
  ];
}
