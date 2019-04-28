{ config, pkgs, ... }:
{
  imports = [
    ../krebs
    ./2configs
    ./3modules
  ];
  nixpkgs.config.packageOverrides = import ./5pkgs pkgs;
  krebs.tinc.retiolum.privkey = {
    source-path = toString <secrets> + "/${config.krebs.tinc.retiolum.netname}.rsa";
    path = "${config.krebs.tinc.retiolum.user.home}/tinc.rsa_key.priv";
    owner = config.krebs.tinc.retiolum.user;
  };
}
