{ pkgs, lib, ... }:
{
  networking.wireless.enable = lib.mkForce false;

  systemd.services.modemmanager = {
    description = "ModemManager";
    after = [ "network-manager.service" ];
    bindsTo = [ "network-manager.service" ];
    wantedBy = [ "network-manager.service" ];
    serviceConfig = {
      ExecStart = "${pkgs.modemmanager}/bin/ModemManager";
      PrivateTmp = true;
      Restart = "always";
      RestartSec = "5";
    };
  };
  networking.networkmanager.enable = true;
  users.users.mainUser = {
    extraGroups = [ "networkmanager" ];
    packages = with pkgs; [
      gnome3.gnome_keyring gnome3.dconf
    ];
  };
}
