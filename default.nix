{ current-date ? abort "current-date not defined"
, current-host-name ? abort "current-host-name not defined"
, current-user-name ? builtins.getEnv "LOGNAME"
}:

assert current-user-name != "";

let
  lib = import <nixpkgs/lib>;

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

  out =
    { inherit (eval {}) pkgs; } //
    lib.mapAttrs
      (name: _:
        if builtins.pathExists (./. + "/${name}/default.nix")
          then import (./. + "/${name}")
          else import-1systems (./. + "/${name}/1systems"))
      (lib.filterAttrs
        (n: t: !lib.hasPrefix "." n && t == "directory")
        (builtins.readDir ./.));

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

  # TODO move to lib
  Just = x: { type = "maybe"; value = x; };
  Nothing = { type = "maybe"; };
  isMaybe = x: builtins.typeOf x == "set" && x.type or false == "maybe";
  isJust = x: isMaybe x && builtins.hasAttr "value" x;
  fromJust = x: assert isJust x; x.value;
  catMaybes = xs: map fromJust (builtins.filter isJust xs);

in out
