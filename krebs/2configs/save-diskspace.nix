{lib, ... }:
# TODO: do not check out nixpkgs master but fetch revision from github
{
  nix.gc.automatic = true;
  nix.gc.dates = lib.mkDefault "03:10";
  documentation.info.enable = false;
  documentation.man.enable = false;
  services.journald.extraConfig = "SystemMaxUse=50M";
  documentation.nixos.enable = false;
}
