{ config, pkgs, ... }:

let
  suspend = pkgs.writeDash "suspend" ''
    ${pkgs.systemd}/bin/systemctl suspend
  '';

  speak = text:
    pkgs.writeDash "speak" ''
      ${pkgs.espeak}/bin/espeak -v +whisper -s 110 "${text}"
    '';

in {
  lass.power-action = {
    enable = true;
    plans.low-battery = {
      upperLimit = 30;
      lowerLimit = 25;
      charging = false;
      action = pkgs.writeDash "warn-low-battery" ''
        ${speak "power level low"}
      '';
    };
    plans.suspend = {
      upperLimit = 10;
      lowerLimit = 0;
      charging = false;
      action = pkgs.writeDash "suspend-wrapper" ''
        /var/setuid-wrappers/sudo ${suspend}
      '';
    };
  };

  users.users.power-action.extraGroups = [
    "audio"
  ];

  security.sudo.extraConfig = ''
    ${config.lass.power-action.user.name} ALL= (root) NOPASSWD: ${suspend}
  '';
}
