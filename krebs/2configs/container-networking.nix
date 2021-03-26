{ lib, ... }:
{
  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["ve-+"];
  networking.nat.externalInterface = lib.mkDefault "et0";
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];
}
