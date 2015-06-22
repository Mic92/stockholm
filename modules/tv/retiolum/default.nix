arg@{ config, pkgs, lib, ... }:

let
  cfg = config.tv.retiolum;
  arg' = arg // { inherit cfg; };
in

{
  options.tv.retiolum = import ./options.nix arg';
  config = lib.mkIf cfg.enable (import ./config.nix arg');
}
