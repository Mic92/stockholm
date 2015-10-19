{ current-date ? abort "current-date not defined"
, current-host-name ? abort "current-host-name not defined"
, current-user-name ? builtins.getEnv "LOGNAME"
}:

assert current-user-name != "";

let
  lib = import <nixpkgs/lib>;
  klib = import ./krebs/4lib { inherit lib; };
in with klib; let

  nspath = ns: p: ./. + "/${ns}/${p}";
  kpath = nspath "krebs";
  upath = nspath current-user-name;

  stockholm = {
    imports = map (f: f "3modules") [ kpath upath ];

    nixpkgs.config.packageOverrides = pkgs:
      let
        kpkgs = import (kpath "5pkgs") { inherit pkgs; };
        upkgs = import (upath "5pkgs") { pkgs = pkgs // kpkgs; };
      in
      kpkgs // upkgs;
  };

  out = {
    inherit (eval {}) config options pkgs;
    krebs = import ./krebs;
    users = lib.mapAttrs
              (name: _:
                if builtins.pathExists (nspath name "default.nix")
                  then import (nspath name "default.nix")
                  else import-1systems (nspath name "1systems"))
              (lib.filterAttrs
                (n: t: !lib.hasPrefix "." n && t == "directory" && n != "krebs")
                (builtins.readDir ./.));
  };

  eval = path: import <nixpkgs/nixos/lib/eval-config.nix> {
    modules = [
      stockholm
      path
    ];
  };

  import-1systems = path: lib.mapAttrs (_: mk-system) (nixDir path);

  mk-system = path: rec {
    inherit (eval path) config options;
    system = config.system.build.toplevel;
    fetch = import ./krebs/0tools/fetch.nix { inherit config lib; };
  };

  nixDir = path:
    builtins.listToAttrs
      (catMaybes
        (lib.mapAttrsToList
          (k: v: {
            directory =
              let p = path + "/${k}/default.nix"; in
              if builtins.pathExists p
                then Just (lib.nameValuePair k p)
                else Nothing;
            regular =
              let p = path + "/${k}"; in
              if lib.hasSuffix ".nix" p
                then Just (lib.nameValuePair (lib.removeSuffix ".nix" k) p)
                else Nothing;
          }.${v} or Nothing)
          (builtins.readDir path)));

in out
