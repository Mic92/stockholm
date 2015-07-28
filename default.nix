{ user-name, system-name }:

let
  lib = import 4lib/krebs {
    lib = import <nixpkgs/lib>;
  };

  eval = import <nixpkgs/nixos/lib/eval-config.nix> {
    inherit lib;
    system = builtins.currentSystem;
    modules = map (p: ./. + "/${p}") [
      "${user-name}/systems/${system-name}.nix"
      "${user-name}/modules"
      "3modules/krebs"
    ];
  };

in

{
  inherit (eval) config options;

  system = eval.config.system.build.toplevel;
}
