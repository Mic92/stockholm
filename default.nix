{ user-name, system-name }:

let

  eval = import <nixpkgs/nixos/lib/eval-config.nix> {
    system = builtins.currentSystem;
    modules = map (p: ./. + "/${p}") [
      "${user-name}/1systems/${system-name}.nix"
      "${user-name}/3modules"
      "krebs/3modules"
    ] ++ [
      ({ lib, pkgs, ... }: {
       _module.args.pkgs =
         (import ./krebs/5pkgs { inherit lib pkgs; }) //
         (import (./. + "/${user-name}/5pkgs") { inherit lib pkgs; });
      })
    ];
  };

in

{
  inherit (eval) config options;

  system = eval.config.system.build.toplevel;
}
