{ pkgs, ... }:

let

  #TODO: tab-completion
  krebs-pass = pkgs.writeDashBin "krebs-pass" ''
    PASSWORD_STORE_DIR=$HOME/.krebs-pass \
    exec ${pkgs.pass}/bin/pass $@
  '';

  krebs-passmenu = pkgs.writeDashBin "krebs-passmenu" ''
    PASSWORD_STORE_DIR=$HOME/.krebs-pass \
    exec ${pkgs.pass}/bin/passmenu $@
  '';

in {
  krebs.per-user.lass.packages = [
    krebs-pass
    krebs-passmenu
  ];
}
