# usage: nix-instantiate --eval --json --read-write-mode --strict ci.nix | jq .
with import ./lib;
let
  pkgs = import <nixpkgs> { overlays = [ (import ./submodules/nix-writers/pkgs) ]; };
  system =
    import <nixpkgs/nixos/lib/eval-config.nix> {
      modules = [{
        imports = [
          ./krebs
          ./krebs/2configs
          ({ config, ... }: {
            krebs.build.host = config.krebs.hosts.test-all-krebs-modules;
          })
        ];
      }];
    }
  ;

  ci-systems = filterAttrs (_: v: v.ci) system.config.krebs.hosts;

  build = host: owner:
  ((import (toString ./. + "/${owner}/krops.nix") { name = host; }).test {target = "${getEnv "HOME"}/stockholm-build";});

in mapAttrs (n: h: build n h.owner.name) ci-systems
