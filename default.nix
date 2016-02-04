{ current-host-name ? abort "current-host-name not defined"
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

  lib = let
    nlib = import (slib.npath "lib");
    klib = import (slib.kpath "4lib") { lib = nlib; };
    slib = rec {
      npath = p: <nixpkgs> + "/${p}";
      kpath = p: ./. + "/krebs/${p}";
      upath = p: ./. + "/${current-user-name}/${p}";
    };
    ulib = let p = slib.upath "4lib"; in
      nlib.optionalAttrs (klib.dir.has-default-nix p)
                         (import p { lib = nlib // klib; });
  in nlib // klib // slib // ulib // builtins;

  inherit (eval {}) pkgs;

  base-module = { config, ... }: {
    imports = builtins.filter lib.dir.has-default-nix (lib.concatLists [
      (map (f: f "2configs") [ lib.upath ])
      (map (f: f "3modules") [ lib.kpath lib.upath ])
    ]);

    krebs.current.enable = true;
    krebs.current.host = config.krebs.hosts.${current-host-name};
    krebs.current.user = config.krebs.users.${current-user-name};

    nixpkgs.config.packageOverrides = pkgs: let
      kpkgs = import (lib.kpath "5pkgs") { inherit lib pkgs; };
      upkgs = import (lib.upath "5pkgs") { inherit lib; pkgs = pkgs // kpkgs; };
    in kpkgs // upkgs;
  };

  eval = config: import (lib.npath "nixos/lib/eval-config.nix") {
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
