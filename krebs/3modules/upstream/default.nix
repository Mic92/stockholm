{ pkgs, lib, ... }:
with lib;

{
  imports =
    map
      (name: ./. + "/${name}")
      (filter
        (name: name != "default.nix" && !hasPrefix "." name)
        (attrNames (builtins.readDir ./.)));
}
