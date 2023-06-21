{
  imports = [
    # Replace upstream xmonad module with one that will be reloaded if changed.
    #
    # This module is intended to be upstreamed once fully tested.
    # The patch to be committed can be obtained using:
    #
    # diff -u <nixpkgs/nixos/modules/services/x11/window-managers/xmonad.nix> \
    #         <stockholm/tv/3modules/window-managers/xmonad.nix>
    #
    {
      disabledModules = [ "services/x11/window-managers/xmonad.nix" ];
      imports = [ ./xmonad.nix ];
      nixpkgs.overlays = [(self: super: {
        writers = super.writers // {
          writeHaskellBin = name: spec:
            super.writers.writeHaskellBin name (builtins.removeAttrs spec ["ghcArgs"]);
        };
      })];
    }
  ];
}
