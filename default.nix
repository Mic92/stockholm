{ user-name, system-name }:

let

  eval = import <nixpkgs/nixos/lib/eval-config.nix> {
    system = builtins.currentSystem;
    modules = map (p: ./. + "/${p}") [
      "${user-name}/1systems/${system-name}.nix"
      "${user-name}/3modules"
      "3modules/krebs"
    ];
  };

in

{
  inherit (eval) config options;

  system = eval.config.system.build.toplevel;
}
