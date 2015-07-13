arg@{ config, lib, pkgs, ... }:

let
  cfg = config.lass.iptables;
  arg' = arg // { inherit cfg; };
in

{
  options.lass.iptables = import ./options.nix arg';
  config = lib.mkIf cfg.enable (import ./config.nix arg');
}
