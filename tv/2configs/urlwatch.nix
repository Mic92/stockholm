{ config, ... }:

{
  krebs.urlwatch = {
    enable = true;
    mailto = config.krebs.users.tv.mail;
    onCalendar = "*-*-* 05:00:00";
    urls = [
      ## nixpkgs maintenance

      # 2014-07-29 when one of the following urls change
      # then we have to update the package

      # ref src/nixpkgs/pkgs/tools/admin/sec/default.nix
      https://api.github.com/repos/simple-evcorr/sec/tags

      # ref src/nixpkgs/pkgs/tools/networking/urlwatch/default.nix
      https://thp.io/2008/urlwatch/

      # 2014-12-20 ref src/nixpkgs/pkgs/tools/networking/tlsdate/default.nix
      https://api.github.com/repos/ioerror/tlsdate/tags

      # 2015-02-18
      # ref ~/src/nixpkgs/pkgs/tools/text/qprint/default.nix
      http://www.fourmilab.ch/webtools/qprint/

      # 2014-09-24 ref https://github.com/4z3/xintmap
      http://www.mathstat.dal.ca/~selinger/quipper/

      # 2014-12-12 remove nixopsUnstable when nixops get's bumped to 1.3
      # ref https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/package-management/nixops/unstable.nix
      http://nixos.org/releases/nixops/

      ## other

      https://nixos.org/channels/nixos-unstable/git-revision

      ## 2014-10-17
      ## TODO update ~/src/login/default.nix
      #http://hackage.haskell.org/package/bcrypt
      #http://hackage.haskell.org/package/cron
      #http://hackage.haskell.org/package/hyphenation
      #http://hackage.haskell.org/package/iso8601-time
      #http://hackage.haskell.org/package/ixset-typed
      #http://hackage.haskell.org/package/system-command
      #http://hackage.haskell.org/package/transformers
      #http://hackage.haskell.org/package/web-routes-wai
      #http://hackage.haskell.org/package/web-page
    ];
  };
}
