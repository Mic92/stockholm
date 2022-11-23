{ config, lib, ... }: {
  config =
    lib.mkMerge
      (lib.mapAttrsToList
        (name: _type: let
          path = ./. + "/${name}";
        in {
          krebs = import path { inherit config; };
        })
        (lib.filterAttrs
          (_name: type: type == "directory")
          (builtins.readDir ./.)));
}
