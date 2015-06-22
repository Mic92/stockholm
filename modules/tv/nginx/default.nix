arg@{ config, pkgs, lib, ... }:

let
  cfg = config.tv.nginx;
  arg' = arg // { inherit cfg; };
in

{
  options.tv.nginx = import ./options.nix arg';
  config = lib.mkIf cfg.enable (import ./config.nix arg');
}
