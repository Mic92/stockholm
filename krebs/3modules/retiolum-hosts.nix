with import <stockholm/lib>;
{ config, pkgs, ... }: {
  nixpkgs.config.packageOverrides = super: {
    retiolum-hosts =
      trace "pkgs.retiolum-hosts is deprecated, use pkgs.krebs-hosts-retiolum instead"
      pkgs.krebs-hosts-retiolum;
  };
}
