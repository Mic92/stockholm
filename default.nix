{ current-date ? abort "current-date not defined"
, current-host-name ? abort "current-host-name not defined"
, current-user-name ? builtins.getEnv "LOGNAME"
, StrictHostKeyChecking ? "yes"
}@args:

let stockholm = {
    inherit krebs;
    inherit users;
    inherit lib;
    inherit pkgs;
  };

  krebs = import ./krebs (args // { inherit lib stockholm; });

  lib =
    let
      lib = import <nixpkgs/lib>;
      klib = import ./krebs/4lib { inherit lib; };
      #ulib = import (./. + "/${current-user-name}/4lib") { lib = lib // klib; };
      ulib = {}; # TODO
    in
    builtins // lib // klib // ulib // rec {
      # TODO move this stuff
      stockholm-path = ./.;
      nspath = ns: p: stockholm-path + "/${ns}/${p}";
    };

  inherit (eval {}) pkgs;

  kpath = lib.nspath "krebs";
  upath = lib.nspath current-user-name;

  base-module = { config, ... }: {
    imports = map (f: f "3modules") [ kpath upath ];

    krebs.current.enable = true;
    krebs.current.host = config.krebs.hosts.${current-host-name};
    krebs.current.user = config.krebs.users.${current-user-name};

    nixpkgs.config.packageOverrides = pkgs:
      let
        kpkgs = import (kpath "5pkgs") { inherit lib pkgs; };
        upkgs = import (upath "5pkgs") { inherit lib; pkgs = pkgs // kpkgs; };
      in
      kpkgs // upkgs;
  };

  eval = config: import <nixpkgs/nixos/lib/eval-config.nix> {
    specialArgs = {
      inherit lib;
    };
    modules = [
      base-module
      config
    ];
  };

  # TODO move user namespaces' to users/, so no exception for krebs/ is needed
  users =
    lib.mapAttrs
      (name: _: eval-all-systems (lib.nspath name "1systems"))
      (lib.filterAttrs
        (n: t: !lib.hasPrefix "." n && t == "directory" && n != "krebs")
        (builtins.readDir ./.));

  eval-all-systems = path:
    lib.mapAttrs'
      (n: _: (lib.nameValuePair (lib.removeSuffix ".nix" n)
                                (eval-system (path + "/${n}"))))
      (builtins.readDir path);

  eval-system = path: rec {
    inherit (eval path) config options;
    system = config.system.build.toplevel;
  };

in stockholm
