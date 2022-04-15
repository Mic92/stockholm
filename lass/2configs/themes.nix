{ config, lib, pkgs, ... }: let

    switch-theme = pkgs.writers.writeDashBin "switch-theme" ''
      set -efux
      if [ "$1" = toggle ]; then
        if [ "$(${pkgs.coreutils}/bin/cat /var/theme/current_theme)" = dark ]; then
          ${placeholder "out"}/bin/switch-theme light
        else
          ${placeholder "out"}/bin/switch-theme dark
        fi
      elif test -e "/etc/themes/$1"; then
        ${pkgs.rsync}/bin/rsync --chown=lass:users -a --delete "/etc/themes/$1/" /var/theme/config/
        echo "$1" > /var/theme/current_theme
        ${pkgs.coreutils}/bin/chown lass:users /var/theme/current_theme
        ${pkgs.procps}/bin/pkill -HUP xsettingsd
      else
        echo "theme $1 not found"
      fi
    '';

in {
  systemd.services.xsettingsd = {
    wantedBy = [ "multi-user.target" ];
    after = [ "display-manager.service" ];
    environment.DISPLAY = ":0";
    serviceConfig = {
      ExecStart = "${pkgs.xsettingsd}/bin/xsettingsd -c /var/theme/config/xsettings.conf";
      User = "lass";
    };
  };
  systemd.tmpfiles.rules = [
    "d /var/theme/ 755 lass users"
  ];
  environment.systemPackages = [
    switch-theme
  ];
  environment.etc = {
    "themes/light/xsettings.conf".text = ''
      Net/ThemeName "Adwaita" 
    '';
    "themes/dark/xsettings.conf".text = ''
      Net/ThemeName "Adwaita-dark" 
    '';
  };
  system.activationScripts.theme.text = ''
    if test -e /var/theme/current_theme; then
      ${switch-theme}/bin/switch-theme "$(cat /var/theme/current_theme)" ||
      ${switch-theme}/bin/switch-theme dark
    else
      ${switch-theme}/bin/switch-theme dark
    fi
  '';
}
