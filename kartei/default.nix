{ config, lib, ... }: let
  removeTemplate =
    # TODO don't remove during CI
    lib.flip builtins.removeAttrs ["template"];
in {
  config =
    lib.mkMerge
      (lib.mapAttrsToList
        (name: _type: let
          path = ./. + "/${name}";
        in {
          krebs = import path { inherit config lib; };
        })
        (removeTemplate
          (lib.filterAttrs
            (_name: type: type == "directory")
            (builtins.readDir ./.))));
}
