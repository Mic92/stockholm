{ pkgs, lib, ... }:
{
  networking.wireless.enable = lib.mkForce false;

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
      gnome3.gnome-keyring
      gnome3.dconf
    ];
  };
  environment.systemPackages = [
    pkgs.nm-dmenu
  ];
}
