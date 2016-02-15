import <nixpkgs/nixos/lib/eval-config.nix> {
  modules = [
    (import <nixpkgs/nixos/lib/from-env.nix> "NIXOS_CONFIG" <nixos-config>)
  ];
}
