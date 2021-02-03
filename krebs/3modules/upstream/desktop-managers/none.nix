# Replace upstream none desktop-manager by a real none, that doesn't pull in
# any dependencies.
with import <stockholm/lib>;
{
  disabledModules = singleton "services/x11/desktop-managers/none.nix";
  config.services.xserver.desktopManager.session = singleton {
    name = "none";
    bgSupport = true;
    start = "";
  };
}
