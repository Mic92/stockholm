import <nixpkgs/nixos/lib/eval-config.nix> {
  modules = [
    (import <nixpkgs/nixos/lib/from-env.nix> "NIXOS_CONFIG" <nixos-config>)
  ];
}
//
{
  lib = import ./lib;
  systems = with import ./lib; let
    ns = getEnv "LOGNAME";
  in
    genAttrs
      (attrNames (filterAttrs (_: eq "directory") (readDir (<stockholm> + "/${ns}/1systems"))))
      (name: let
        config = import (<stockholm> + "/${ns}/1systems/${name}/config.nix");
      in import <nixpkgs/nixos/lib/eval-config.nix> {
        modules = [ config ];
      });
}
