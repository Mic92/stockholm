{ config, pkgs, ... }:

let
  suspend = pkgs.writeDash "suspend" ''
    ${pkgs.systemd}/bin/systemctl suspend
  '';

in {
  lass.power-action = {
    enable = true;
    plans.suspend = {
      upperLimit = 10;
      lowerLimit = 0;
      charging = false;
      action = pkgs.writeDash "suspend-wrapper" ''
        /var/setuid-wrappers/sudo ${suspend}
      '';
    };
  };
  security.sudo.extraConfig = ''
    ${config.lass.power-action.user.name} ALL= (root) NOPASSWD: ${suspend}
  '';
}
