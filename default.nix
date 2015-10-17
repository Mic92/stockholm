{ current-date
, current-host-name
, current-user-name
}:

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
    lib.mapAttrs (_: builtins.getAttr "main")
      (lib.filterAttrs (_: builtins.hasAttr "main")
        (lib.mapAttrs
          (k: v:
            if lib.hasPrefix "." k || v != "directory" then
              {}
            else if builtins.pathExists (./. + "/${k}/default.nix") then
              { main = import (./. + "/${k}"); }
            else if builtins.pathExists (./. + "/${k}/1systems") then
              { main = mk-namespace (./. + "/${k}"); }
            else
              {})
          (builtins.readDir ./.)));

  eval = path: import <nixpkgs/nixos/lib/eval-config.nix> {
    system = builtins.currentSystem;
    modules = [
      stockholm
      path
    ];
  };

  mk-namespace = path: mapNixDir mk-system (path + "/1systems");

  mk-system = path: rec {
    inherit (eval path) config options;
    system = config.system.build.toplevel;
    fetch = import ./krebs/0tools/fetch.nix { inherit config lib; };
  };

  mapNixDir = f: path: lib.mapAttrs (_: f) (nixDir path);

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
