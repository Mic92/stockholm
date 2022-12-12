with import ./lib;
{ config, pkgs, ... }: {
  environment.etc.gitconfig.text = ''
    [alias]
      patch = !${pkgs.git}/bin/git --no-pager diff --no-color
    [diff-so-fancy]
      markEmptyLines = false
      stripLeadingSymbols = false
    [pager]
      diff = ${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy \
           | ${pkgs.less}/bin/less -FRX
    [user]
      email = tv@krebsco.de
      name = tv
  '';
}
