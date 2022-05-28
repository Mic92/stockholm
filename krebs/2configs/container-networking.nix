{ lib, ... }:
{
  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["ve-+" "ctr+" ];
  networking.nat.externalInterface = lib.mkDefault "et0";
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];
}
