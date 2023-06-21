{ config, pkgs, lib, ... }:
with lib;
{
  options = {
    services.xserver.desktopManager.coma = {
      enable = mkEnableOption "sleep as a desktop manager";
    };
  };
  config = mkIf config.services.xserver.desktopManager.coma.enable {
    services.xserver.desktopManager.session = singleton {
      name = "coma";
      bgSupport = true;
      start = /* sh */ ''
        if test -n "$waitPID"; then
          ${pkgs.uutils-coreutils}/bin/uutils-sleep 1s && kill $waitPID &
          wait $waitPID
        fi
        exec -a sleep ${pkgs.uutils-coreutils}/bin/uutils-sleep infinity
      '';
    };
  };
}
