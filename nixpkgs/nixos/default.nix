{ configuration ? import <upstream-nixpkgs/nixos/lib/from-env.nix> "NIXOS_CONFIG" <nixos-config>
, system ? builtins.currentSystem
}:

let
  eval-config = modules: import <upstream-nixpkgs/nixos/lib/eval-config.nix> {
    inherit system;
    modules = modules ++ [({ config, lib, ... }: with lib; {
      imports = filter dir.has-default-nix (concatLists [
        (map (p: p + "/2configs") [ <stockholm-private> ])
        (map (p: p + "/3modules") [ <stockholm-krebs> <stockholm-private> ])
      ]);

      krebs.current = {
        enable = true;
        host = config.krebs.hosts.${readFile /proc/sys/kernel/hostname};
        user = config.krebs.users.${getEnv "LOGNAME"};
      };

      nixpkgs.config.packageOverrides = pkgs: let
        kpkgs = import <stockholm-krebs/5pkgs> { inherit lib pkgs; };
        upkgs = import <stockholm-private/5pkgs> { inherit lib; pkgs = pkgs // kpkgs; };
      in kpkgs // upkgs;
    })];
    specialArgs = {
      lib = let
        nlib = import <upstream-nixpkgs/lib> // builtins;
        klib = nlib // import <stockholm-krebs/4lib> { lib = nlib; };
        ulib = klib // (with klib; let p = <stockholm-private> + "/4lib"; in
          optionalAttrs (dir.has-default-nix p)
                        (import p { lib = klib; }));
      in ulib;
    };
  };

  eval = eval-config [
    configuration
  ];

  # This is for `nixos-rebuild build-vm'.
  vm = eval-config [
    configuration
    <upstream-nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
  ];

  # This is for `nixos-rebuild build-vm-with-bootloader'.
  vm-with-bootloader = eval-config [
    configuration
    <upstream-nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
    { virtualisation.useBootLoader = true; }
  ];
in

{
  inherit (eval) config options;

  system = eval.config.system.build.toplevel;

  vm = vm.config.system.build.vm;

  vmWithBootLoader = vm-with-bootloader.config.system.build.vm;

  # The following are used by nixos-rebuild.
  nixFallback = eval.pkgs.nixUnstable;
}
