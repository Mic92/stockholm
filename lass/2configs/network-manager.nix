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
  networking.networkmanager = {
    ethernet.macAddress = "random";
    wifi.macAddress = "random";
    enable = true;
    unmanaged = [
      "docker*"
      "vboxnet*"
    ];
  };
  users.users.mainUser = {
    extraGroups = [ "networkmanager" ];
    packages = with pkgs; [
      gnome3.gnome_keyring
      gnome3.dconf
    ];
  };
  environment.systemPackages = [
    pkgs.nm-dmenu
  ];
}
