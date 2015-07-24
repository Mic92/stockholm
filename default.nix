{ user-name, system-name }:

let

	eval = import <nixpkgs/nixos/lib/eval-config.nix> {
    system = builtins.currentSystem;
		modules = [
      (./1systems + "/${user-name}/${system-name}.nix")
			(./3modules/krebs)
			(./3modules + "/${user-name}")
		];
	};

in

{
	inherit (eval) config options;

	system = eval.config.system.build.toplevel;
}
