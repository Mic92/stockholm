{ config, pkgs, ... }:
let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  environment.systemPackages = with pkgs; [
    cabal2nix
    gcc
    ghc
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stack
    haskellPackages.stylish-haskell
    (all-hies.selection { selector = p: {inherit (p) ghc864; }; })
  ];
}
