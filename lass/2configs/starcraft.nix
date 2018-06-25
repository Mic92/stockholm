{ config, pkgs, ... }: let
  mainUser = config.users.extraUsers.mainUser;
  newWine = pkgs.wineStaging;
  #newWine = pkgs.wineStaging.overrideAttrs (old: {
  #  name = "wine-3.7";
  #  buildInputs = old.buildInputs ++ [
  #    pkgs.libuuid.bin
  #    pkgs.autoconf.out
  #  ];
  #  src = pkgs.fetchurl {
  #    url = "https://dl.winehq.org/wine/source/3.x/wine-3.7.tar.xz";
  #    sha256 = "1drbzk3y0m14lkq3vzwwkvain5shykgcbmyzh6gcb5r4sxh3givn";
  #  };
  #  postPatch = old.postPatch or "" + ''
  #    patchShebangs tools
  #    cp -r ${pkgs.fetchFromGitHub {
  #      sha256 = "0kam73jqhah7bzji5csxxhhfdp6byhzpcph6xnzjqz2aic5xk7xi";
  #      owner = "wine-staging";
  #      repo = "wine-staging";
  #      rev = "v3.7";
  #    }}/patches .
  #    chmod +w patches
  #    cd patches
  #    patchShebangs gitapply.sh
  #    ./patchinstall.sh DESTDIR="$PWD/.." --all
  #    cd ..
  #  '';

  #});
  #newWine = (import (builtins.fetchGit {
  #  url = "https://github.com/NixOS/nixpkgs";
  #  rev = "696c6bed4e8e2d9fd9b956dea7e5d49531e9d13f";
  #}) {}).wineStaging;
in {
  users.users= {
    starcraft = {
      isNormalUser = true;
      extraGroups = [
        "audio"
        "video"
      ];
      packages = [
        newWine
        pkgs.winetricks
        pkgs.mpg123
      ];
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(starcraft) NOPASSWD: ALL
  '';
}

