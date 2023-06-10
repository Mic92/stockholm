{ lib, ... }:
# Replace upstream none desktop-manager by a real none, that doesn't pull in
# any dependencies.
{
  disabledModules = lib.singleton "services/x11/desktop-managers/none.nix";
  config.services.xserver.desktopManager.session = lib.singleton {
    name = "none";
    bgSupport = true;
    start = "";
  };
}
