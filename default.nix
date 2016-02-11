{ configuration ? import (nixpkgs-path + "/nixos/lib/from-env.nix") "NIXOS_CONFIG" <nixos-config>
, system ? builtins.currentSystem
, current-host-name ?
    let v = builtins.getEnv "HOSTNAME"; in
    if v != "" then v else builtins.readFile /proc/sys/kernel/hostname
, current-user-name ?
    let v = builtins.getEnv "LOGNAME"; in
    if v != "" then v else abort "undefined variable: LOGNAME"
, nixpkgs-path ?
    if (builtins.tryEval <nixpkgs/krebs>).success
      then <upstream-nixpkgs>
      else <nixpkgs>
, StrictHostKeyChecking ? "yes"
}@args:

let stockholm = {
    inherit krebs;
    inherit users;
    inherit lib;
    inherit config options pkgs;
    system = config.system.build.toplevel;
  };

  krebs = import ./krebs (args // { inherit lib stockholm; });

  lib = let
    nlib = import (slib.npath "lib");
    klib = import (slib.kpath "4lib") { lib = nlib; };
    slib = rec {
      nspath = ns: p: ./. + "/${ns}/${p}";
      npath = p: nixpkgs-path + "/${p}";
      kpath = nspath "krebs";
      upath = nspath current-user-name;
    };
    ulib = let p = slib.upath "4lib"; in
      nlib.optionalAttrs (klib.dir.has-default-nix p)
                         (import p { lib = nlib // klib; });
  in nlib // klib // slib // ulib // builtins;

  inherit (eval configuration) config options pkgs;

  base-module = { config, ... }: {
    imports = builtins.filter lib.dir.has-default-nix (lib.concatLists [
      (map (f: f "2configs") [ lib.upath ])
      (map (f: f "3modules") [ lib.kpath lib.upath ])
      (map (f: f "5pkgs") [ lib.kpath lib.upath ])
    ]);

    krebs.current.enable = true;
    krebs.current.host = config.krebs.hosts.${current-host-name};
    krebs.current.user = config.krebs.users.${current-user-name};
  };

  eval = config: import (lib.npath "nixos/lib/eval-config.nix") {
    inherit system;
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
